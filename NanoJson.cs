///////////////////////////////////////////////////////////////
///															///
///		NanoJson by Duncan 'ScoredOne' Mellor				///
///															///
///		Released under the MIT license						///
///															///
///		Software provided in as-is condition				///
///															///
///		Git: https://github.com/ScoredOne/NanoJson			///
///															///
///		Nuget: https://www.nuget.org/packages/NanoJson		///
///		                                                    ///
///     Base: NetStandard 2.1 C# 8                          ///
///                                                         ///
///     Version: 1.4.0                                      ///
///															///
///////////////////////////////////////////////////////////////

using System;
using System.Buffers;
using System.Collections.Concurrent;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

using static ScoredProductions.NanoJson.NanoJsonStatics;

namespace ScoredProductions.NanoJson {

    #region ### JsonSpan ###

    [StructLayout(LayoutKind.Sequential)]
    public ref struct JsonSpan {

        public static JsonSpan Empty => new JsonSpan(true);

        public readonly bool IsEmpty;
        public readonly JsonType Type;
        public readonly ReadOnlySpan<char> Key;
        public readonly ReadOnlySpan<char> Value;

        private bool currentIsEmpty;
        private int index;
        private JsonType currentType;
        private JsonReader reader;
        private ReadOnlySpan<char> currentKey;
        private ReadOnlySpan<char> currentValue;


        public readonly JsonSpan Current => new JsonSpan(in this.currentKey, in this.currentValue, in this.currentType, in this.currentIsEmpty);
        public readonly bool IsNothing => this.Key.IsEmpty && this.Value.IsEmpty;

        public readonly bool IsString => HasFlag((long)this.Type, (long)JsonType.String);
        public readonly bool IsBool => HasFlag((long)this.Type, (long)JsonType.Boolean);
        public readonly bool IsNumber => HasFlag((long)this.Type, (long)JsonType.Number);
        public readonly bool IsNull => HasFlag((long)this.Type, (long)JsonType.Null);
        public readonly bool IsArray => HasFlag((long)this.Type, (long)JsonType.Array);
        public readonly bool IsObject => HasFlag((long)this.Type, (long)JsonType.Object);
        public readonly bool IsValue => this.IsString || this.IsBool || this.IsNumber || this.IsNull;
        public readonly bool IsContainer => this.IsArray || this.IsObject;

        private JsonSpan(bool _) {
            this.Type = JsonType.Null;
            this.currentType = 0;
            this.Key = ReadOnlySpan<char>.Empty;
            this.Value = ReadOnlySpan<char>.Empty;
            this.IsEmpty = true;

            this.currentIsEmpty = true;
            this.reader = JsonReader.Empty;
            this.currentKey = ReadOnlySpan<char>.Empty;
            this.currentValue = ReadOnlySpan<char>.Empty;
            this.index = -1;
        }

        /// <summary>
        /// Constructor for this.Current only
        /// </summary>
        /// <param name="key"></param>
        /// <param name="read"></param>
        /// <param name="type"></param>
        /// <param name="empty"></param>
        private JsonSpan(in ReadOnlySpan<char> key, in ReadOnlySpan<char> read, in JsonType type, in bool empty) {
            this.Type = type;
            this.Key = key;
            this.Value = read;
            this.IsEmpty = empty;

            switch (this.Type) {
                case JsonType.Array:
                    this.reader = new JsonReader(this.Value);
                    this.reader.AdvanceTo(LBRACKET);
                    break;
                case JsonType.Object:
                    this.reader = new JsonReader(this.Value);
                    this.reader.AdvanceTo(LBRACE);
                    break;
                default:
                    this.reader = JsonReader.Empty;
                    break;
            }

            this.currentIsEmpty = true;
            this.currentKey = ReadOnlySpan<char>.Empty;
            this.currentValue = ReadOnlySpan<char>.Empty;
            this.currentType = 0;
            this.index = -1;
        }

        public JsonSpan(string data) : this(data.AsSpan()) { }
        public JsonSpan(in ReadOnlySpan<char> read) : this(ReadOnlySpan<char>.Empty, read) { }

        public JsonSpan(string key, string value) : this(key.AsSpan(), value.AsSpan()) { }
        public JsonSpan(in ReadOnlySpan<char> key, in ReadOnlySpan<char> read) {
            JsonReader reader = new JsonReader(in read);
            this = new JsonSpan(in key, ref reader, false);
        }

        /// <summary>
        /// Enumerator / JsonReader Specific constructor
        /// </summary>
        /// <param name="key"></param>
        /// <param name="reader"></param>
        /// <exception cref="ArgumentException"></exception>
        private JsonSpan(in ReadOnlySpan<char> key, ref JsonReader reader, bool continuous) {
            this.currentKey = ReadOnlySpan<char>.Empty;
            this.currentValue = ReadOnlySpan<char>.Empty;
            this.currentType = 0;
            this.currentIsEmpty = true;
            this.index = -1;
            this.Key = key;

            if (TryParseValueFromReader(ref reader, continuous, out this.Type, out this.Value, out this.IsEmpty)) {
                if (HasFlag((long)this.Type, (long)JsonType.Object)) {
                    this.reader = new JsonReader(this.Value);
                    this.reader.AdvanceTo(LBRACE);
                }
                else if (HasFlag((long)this.Type, (long)JsonType.Array)) {
                    this.reader = new JsonReader(this.Value);
                    this.reader.AdvanceTo(LBRACKET);
                }
                else {
                    this.reader = JsonReader.Empty;
                }
            }
            else {
                this = JsonSpan.Empty;
                this.Key = key;
            }
        }

        private static bool TryParseValueFromReader(ref JsonReader reader, bool continuous, out JsonType type, out ReadOnlySpan<char> value, out bool isEmpty) {
            if (!reader.CanAdvance) {
                type = JsonType.Null;
                value = ReadOnlySpan<char>.Empty;
                isEmpty = true;
                return false;
            }

            ushort first;
            if (reader.CurrentIndex < 0 || IsWhiteSpace(reader.CurrentValue)) {
                first = reader.AdvanceToNotWhiteSpace();
            }
            else {
                first = reader.CurrentValue;
            }

            switch (first) {
                case QUOTE: {
                    type = JsonType.String;
                    int left = reader.CurrentIndex + 1;
                    int right;
                    if (continuous) {
                        right = reader.AdvanceTo(QUOTE);
                    }
                    else {
                        reader.SetIndexToEnd();
                        right = reader.RetreatTo(QUOTE);
                    }
                    reader.Increment();
                    if (left == right) {
                        value = ReadOnlySpan<char>.Empty;
                        isEmpty = true;
                    }
                    else {
                        value = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, right - left));
                        isEmpty = false;
                    }
                    return true;
                }
                case LBRACKET: {
                    type = JsonType.Array;
                    int left = reader.CurrentIndex;
                    if (reader.AdvanceToNotWhiteSpace() == RBRACKET) {
                        isEmpty = true;
                    }
                    else {
                        isEmpty = false;
                        if (continuous) {
                            reader.SetIndexPosition(left);
                            reader.AdvanceToEndOfArray();
                        }
                        else {
                            reader.SetIndexToEnd();
                            reader.RetreatTo(RBRACKET);
                        }
                        if (reader.CurrentValue != RBRACKET) {
                            throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                        }
                    }
                    int right;
                    if (reader.Increment()) {
                        right = reader.CurrentIndex - left;
                    }
                    else {
                        right = reader.CurrentIndex - left + 1;
                    }
                    ReadOnlySpan<ushort> container = reader.Slice(left, right);
                    value = MemoryMarshal.Cast<ushort, char>(container);
                    return true;
                }
                case LBRACE: {
                    type = JsonType.Object;
                    int left = reader.CurrentIndex;
                    if (reader.AdvanceToNotWhiteSpace() == RBRACE) {
                        isEmpty = true;
                    }
                    else {
                        isEmpty = false;
                        if (continuous) {
                            reader.SetIndexPosition(left);
                            reader.AdvanceToEndOfObject();
                        }
                        else {
                            reader.SetIndexToEnd();
                            reader.RetreatTo(RBRACE);
                        }
                        if (reader.CurrentValue != RBRACE) {
                            throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                        }
                    }
                    int right;
                    if (reader.Increment()) {
                        right = reader.CurrentIndex - left;
                    }
                    else {
                        right = reader.CurrentIndex - left + 1;
                    }
                    ReadOnlySpan<ushort> container = reader.Slice(left, right);
                    value = MemoryMarshal.Cast<ushort, char>(container);
                    return true;
                }
                case N_LOWER:
                case N_UPPER: {
                    type = JsonType.Null;
                    value = NULL.AsSpan();
                    isEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);

                    if (right == 4) {
                        ReadOnlySpan<ushort> data = reader.Slice(left, right);
                        ushort c = data[1];
                        if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                            c = data[2];
                            if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                c = data[3];
                                if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                    return true;
                                }
                            }
                        }
                    }

                    throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                }
                case T_LOWER:
                case T_UPPER: {
                    type = JsonType.Boolean;
                    value = bool.TrueString.AsSpan();
                    isEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);

                    if (right == 4) {
                        ReadOnlySpan<ushort> toCompare = reader.Slice(left, right);
                        ushort c = toCompare[1];
                        if ((c ^ R_LOWER) == 0 || (c ^ R_UPPER) == 0) {
                            c = toCompare[2];
                            if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                                c = toCompare[3];
                                if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                    return true;
                                }
                            }
                        }
                    }

                    throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                }
                case F_LOWER:
                case F_UPPER: {
                    type = JsonType.Boolean;
                    value = bool.FalseString.AsSpan();
                    isEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);

                    if (right == 5) {
                        ReadOnlySpan<ushort> data = reader.Slice(left, right);
                        ushort c = data[1];
                        if ((c ^ A_LOWER) == 0 || (c ^ A_UPPER) == 0) {
                            c = data[2];
                            if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                c = data[3];
                                if ((c ^ S_LOWER) == 0 || (c ^ S_UPPER) == 0) {
                                    c = data[4];
                                    if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                        return true;
                                    }
                                }
                            }
                        }
                    }

                    throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                }
                default: {
                    type = JsonType.Number;
                    isEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);
                    value = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, right));

                    if (IsNumber(value)) {
                        return true;
                    }
                    throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                }
            }

            throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
        }

        /// <summary>
        /// Single time access of index, work done after aquiring value is lost, use Enumerator for more persistent access
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        /// <exception cref="IndexOutOfRangeException"></exception>
        public readonly JsonSpan this[in int index]
        {
            get
            {
                if (this.IsContainer) {
                    ref readonly JsonSpan t = ref this;
                    if (t.TryGetIndex(in index, out JsonSpan v)) {
                        return v;
                    }
                }
                throw new IndexOutOfRangeException();
            }
        }

        public bool MoveNext() {
            if (this.IsObject) {
                ref JsonReader providedReader = ref this.reader;
                if (!providedReader.CanAdvance) {
                    this.currentKey = ReadOnlySpan<char>.Empty;
                    this.currentValue = ReadOnlySpan<char>.Empty;
                    return false;
                }
                providedReader.AdvanceToNotWhiteSpace();
                if (providedReader.CurrentValue != QUOTE) {
                    providedReader.AdvanceTo(QUOTE);
                }
                int left;
                if (providedReader.Advance() == QUOTE) {
                    this.currentKey = ReadOnlySpan<char>.Empty;
                }
                else {
                    left = providedReader.CurrentIndex;
                    providedReader.AdvanceTo(QUOTE);
                    this.currentKey = this.Value.Slice(left, providedReader.CurrentIndex - left);
                }
                providedReader.AdvanceTo(COLON);
                providedReader.AdvanceToNotWhiteSpace();
                left = providedReader.CurrentIndex;
                this.index++;
                TryParseValueFromReader(ref providedReader, true, out this.currentType, out this.currentValue, out this.currentIsEmpty);
                switch (providedReader.CurrentValue) {
                    case COMMA:
                    case RBRACE:
                    case RBRACKET:
                        break;
                    default: {
                        providedReader.AdvanceToNotWhiteSpace();
                        break;
                    }
                }
                if (left == providedReader.CurrentIndex) {
                    return false;
                }
                return true;
            }
            else if (this.IsArray) {
                ref JsonReader providedReader = ref this.reader;
                if (!providedReader.CanAdvance) {
                    this.currentValue = ReadOnlySpan<char>.Empty;
                    return false;
                }
                providedReader.AdvanceToNotWhiteSpace();
                int left = providedReader.CurrentIndex;
                this.index++;
                TryParseValueFromReader(ref providedReader, true, out this.currentType, out this.currentValue, out this.currentIsEmpty);
                switch (providedReader.CurrentValue) {
                    case COMMA:
                    case RBRACE:
                    case RBRACKET:
                        break;
                    default: {
                        providedReader.AdvanceToNotWhiteSpace();
                        break;
                    }
                }
                if (left == providedReader.CurrentIndex) {
                    return false;
                }
                return true;
            }
            return false;
        }

        /// <summary>
        /// Resets the Enumeration search
        /// </summary>
        public void Reset() {
            this.currentIsEmpty = false;
            this.currentType = 0;
            this.currentKey = ReadOnlySpan<char>.Empty;
            this.currentValue = ReadOnlySpan<char>.Empty;
            this.index = -1;
            this.reader.SetIndexToStart();
            if (this.IsObject) {
                this.reader.AdvanceTo(LBRACE);
            }
            else if (this.IsArray) {
                this.reader.AdvanceTo(LBRACKET);
            }
        }

        public bool TryGetIndex(in int index, out JsonSpan value) {
            if (index < 0) {
                throw new ArgumentOutOfRangeException(nameof(index));
            }
            if (index == this.index) {
                value = this.Current;
                return true;
            }
            int endLen = this.index;
            if (this.index > 0) {
                while (this.MoveNext()) {
                    if (this.index == index) {
                        value = this.Current;
                        return true;
                    }
                }
                this.Reset();
                for (int x = 0; x < endLen; x++) {
                    this.MoveNext();
                    if (this.index == index) {
                        value = this.Current;
                        return true;
                    }
                }
            }
            else {
                while (this.MoveNext()) {
                    if (this.index == index) {
                        value = this.Current;
                        return true;
                    }
                }
            }
            value = Empty;
            return false;
        }

        public readonly JsonSpan this[in string key] => this[key.AsSpan()];

        public readonly JsonSpan this[in ReadOnlySpan<char> key]
        {
            get
            {
                if (this.IsObject) {
                    ref readonly JsonSpan t = ref this;
                    if (t.TryGetKey(in key, out JsonSpan v)) {
                        return v;
                    }
                }
                return Empty;
            }
        }

        public bool TryGetKey(in ReadOnlySpan<char> key, out JsonSpan value) {
            if (key.IsEmpty) {
                throw new ArgumentOutOfRangeException(nameof(key));
            }
            if (!this.IsObject) {
                value = Empty;
                return false;
            }

            int keyhash = ComputeHash(in key, out int pathLen);
            int valueKey;
            int valueLen;
            int endLen = this.index;
            if (endLen < 0) {
                if (!this.MoveNext()) {
                    value = Empty;
                    return false;
                }
            }
            JsonSpan current;
            do {
                current = this.Current;
                valueKey = ComputeHash(current.Key, out valueLen);
                if (keyhash == valueKey) {
                    value = current;
                    return true;
                }
                else {
                    if (pathLen > valueLen && ComputeHash(key.Slice(0, valueLen), out _) == valueKey) {
                        if (current.TryGetKey(key[++valueLen..], out value)) {
                            return true;
                        }
                    }
                }
            } while (this.MoveNext());
            if (endLen > 0) {
                this.Reset();
                for (int x = 0; x < endLen; x++) {
                    this.MoveNext();
                    current = this.Current;
                    valueKey = ComputeHash(current.Key, out valueLen);
                    if (keyhash == valueKey) {
                        value = current;
                        return true;
                    }
                    else {
                        if (pathLen > valueLen && ComputeHash(key.Slice(0, valueLen), out _) == valueKey) {
                            if (current.TryGetKey(key[++valueLen..], out value)) {
                                return true;
                            }
                        }
                    }
                }
            }
            value = Empty;
            return false;
        }

        public readonly JsonSpan GetEnumerator() => this;

        /// <summary>
        /// Get the length of the reference area
        /// </summary>
        public readonly int GetLength => this.Value.Length;

        public readonly int KeyLen => this.Key.Length;

        /// <summary>
        /// Get the string value as-is in relation to this object
        /// </summary>
        public readonly string GetStringLiteral => this.Value.ToString();

        public readonly int GetTranslatedUnicodeLength => GetStringDecodeLengthFromSpan(this.Value);

        /// <summary>
        /// Get the decoded string value of the object
        /// </summary>
        public readonly string GetStringDecoded => GetStringDecodedFromSpan(in this.Value);

        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public string TryGetString(in ReadOnlySpan<char> key, bool decoded = true) {
            return this.TryGetKey(in key, out JsonSpan value) && value.IsString ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
        }
        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetString(in ReadOnlySpan<char> key, out string @out, bool decoded = true) {
            if (this.TryGetKey(key, out JsonSpan value) && value.IsString) {
                @out = decoded ? value.GetStringDecoded : value.GetStringLiteral;
                return true;
            }
            else {
                @out = string.Empty;
                return false;
            }
        }

        public override string ToString() => this.ToString(Default_ToStringFormat);

        public string ToString(ToStringFormat format) {
            int indent = 0;
            int pos = 0;

            bool pretty = HasFlag((long)format, (long)ToStringFormat.Pretty);
            bool translateUnicode = HasFlag((long)format, (long)ToStringFormat.TranslateUnicode);
            bool lowerCaseBool = HasFlag((long)format, (long)ToStringFormat.LowerCaseBool);
            bool reparseNumbers = HasFlag((long)format, (long)ToStringFormat.ReParseNumbers);

            if (this.IsString) {
                if (translateUnicode) {
                    return this.GetStringDecoded;
                }
                else {
                    return this.GetStringLiteral;
                }
            }
            else if (this.IsNull) {
                if (HasFlag((long)format, (long)ToStringFormat.NullReturnsEmptyReference)) {
                    return null;
                }
                else {
                    return NULL;
                }
            }
            else if (this.IsNumber) {
                if (reparseNumbers) {
                    return this.GetNumberString;
                }
                else {
                    return this.GetStringLiteral;
                }
            }
            else if (this.IsBool) {
                if (MemoryMarshal.GetReference(this.Value) == 'T') {
                    return lowerCaseBool ? TRUE_l : TRUE_u;
                }
                else {
                    return lowerCaseBool ? FALSE_l : FALSE_u;
                }
            }

            char[] buffer = ArrayPool<char>.Shared.Rent((int)Math.Min((double)this.GetLength * (INDENT_LEN * 4), int.MaxValue));
            this.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, buffer.AsSpan(), ref indent, ref pos, INDENT_TABS.AsSpan());
            string builtString = string.Create(pos, (buffer, pos), (span, state) => state.buffer.AsSpan(0, state.pos).CopyTo(span));
            ArrayPool<char>.Shared.Return(buffer);
            return builtString;
        }

        /// <summary>
        /// Recursive method to build the json ToString output
        /// </summary>
        private void ProcessString(bool AsValue, in bool pretty, in bool translateUnicode, in bool lowerCaseBool, in bool reparseNumbers, in Span<char> sb, ref int indent, ref int sbPos, in ReadOnlySpan<char> indentSpan) {
            if (this.IsString) {
                sb[sbPos++] = '"';
                if (translateUnicode) {
                    TranslateUnicodeIntoBufferFromSpan(this.Value, in sb, ref sbPos);
                }
                else {
                    this.Value.CopyTo(sb.Slice(sbPos, this.GetLength));
                    sbPos += this.GetLength;
                }
                sb[sbPos++] = '"';
            }
            else if (this.IsNull) {
                this.Value.CopyTo(sb.Slice(sbPos, 4));
                sbPos += 4;
            }
            else if (this.IsNumber) {
                if (reparseNumbers) {
                    this.GetNumber.TryFormat(sb.Slice(sbPos), out int refSpanLen);
                    sbPos += refSpanLen;
                }
                else {
                    this.Value.CopyTo(sb.Slice(sbPos, this.GetLength));
                    sbPos += this.GetLength;
                }
            }
            else if (this.IsBool) {
                this.Value.CopyTo(sb.Slice(sbPos, this.GetLength));
                if (lowerCaseBool) {
                    switch (MemoryMarshal.GetReference(this.Value)) {
                        case 'F':
                            sb[sbPos] = 'f';
                            break;
                        case 'T':
                            sb[sbPos] = 't';
                            break;
                    }
                }
                sbPos += this.GetLength;
            }
            else if (this.IsObject) {
                int x;
                int y;
                if (pretty && !AsValue) {
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }

                sb[sbPos++] = '{';
                this.Reset();
                bool moved = this.MoveNext();
                if (this.IsEmpty || !moved) {
                    if (pretty) {
                        sb[sbPos++] = ' ';
                    }
                    sb[sbPos++] = '}';
                    return;
                }

                if (pretty) {
                    indent++;
                    sb[sbPos++] = '\n';

                    do {
                        JsonSpan current = this.Current;
                        for (y = indent; --y >= 0;) {
                            indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                        sb[sbPos++] = '"';
                        current.Key.CopyTo(sb.Slice(sbPos, current.KeyLen));
                        sbPos += current.KeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        current.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, in sb, ref indent, ref sbPos, in indentSpan);
                        moved = this.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                        sb[sbPos++] = '\n';
                    } while (moved);

                    indent--;
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }
                else {
                    do {
                        JsonSpan current = this.Current;
                        sb[sbPos++] = '"';
                        current.Key.CopyTo(sb.Slice(sbPos, current.KeyLen));
                        sbPos += current.KeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        current.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, in sb, ref indent, ref sbPos, in indentSpan);
                        moved = this.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                    } while (moved);
                }

                sb[sbPos++] = '}';
            }
            else if (this.IsArray) {
                int x;
                int y;
                if (pretty && !AsValue) {
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }
                sb[sbPos++] = '[';
                this.Reset();
                bool moved = this.MoveNext();
                if (this.IsEmpty || !moved) {
                    if (pretty) {
                        sb[sbPos++] = ' ';
                    }
                    sb[sbPos++] = ']';
                    return;
                }

                if (pretty) {
                    indent++;
                    sb[sbPos++] = '\n';

                    do {
                        JsonSpan current = this.Current;
                        if (current.IsValue) {
                            for (y = indent; --y >= 0;) {
                                indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                                sbPos += INDENT_LEN;
                            }
                        }
                        current.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, in sb, ref indent, ref sbPos, in indentSpan);
                        moved = this.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                        sb[sbPos++] = '\n';
                    } while (moved);

                    indent--;
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }
                else {
                    do {
                        this.Current.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, in sb, ref indent, ref sbPos, in indentSpan);
                        moved = this.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                    } while (moved);
                }

                sb[sbPos++] = ']';
            }
        }

        /// <summary>
        /// Get the number contained inside This object
        /// </summary>
        public readonly double GetNumber
        {
            get
            {
                double returnNumber = double.NaN;
                if (this.IsNumber) {
                    returnNumber = ParseNumber(this.Value);
                }
                return returnNumber;
            }
        }

        /// <summary>
        /// Get the number contained inside This object and parse it tostring
        /// </summary>
        private readonly string GetNumberString => this.GetNumber.ToString();

        /// <summary>
        /// Get the number contained inside This object
        /// </summary>
        public readonly T GetNumberOfType<T>() where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            return double.TryParse(this.Value, out double value) ? GetConvertedValue<T>(value) : default;
        }

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public double TryGetNumber(string key) => this.TryGetNumber(key.AsSpan());
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetNumber(string key, out double @out) => this.TryGetNumber(key.AsSpan(), out @out);

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public double TryGetNumber(in ReadOnlySpan<char> key) {
            return this.TryGetNumber(in key, out double value) ? value : double.NaN;
        }
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetNumber(in ReadOnlySpan<char> key, out double @out) {
            if (this.TryGetKey(in key, out JsonSpan value) && value.IsNumber) {
                return double.TryParse(value.Value, out @out);
            }
            else {
                @out = double.NaN;
                return false;
            }
        }

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public T TryGetNumber<T>(string key) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            return this.TryGetNumber<T>(key.AsSpan());
        }
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetNumber<T>(string key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            return this.TryGetNumber(key.AsSpan(), out @out);
        }

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public T TryGetNumber<T>(in ReadOnlySpan<char> key) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            this.TryGetNumber(in key, out T value);
            return value;
        }
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetNumber<T>(in ReadOnlySpan<char> key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            if (this.TryGetKey(in key, out JsonSpan value) && value.IsNumber) {
                @out = value.GetNumberOfType<T>();
                return true;
            }
            else {
                @out = default;
                return false;
            }
        }

        /// <summary>
        /// Get the bool value of This object
        /// </summary>
        public readonly bool GetBool => bool.TryParse(this.Value, out bool value) && value;

        /// <summary>
        /// Try to get the bool value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetBool(string key) {
            return this.TryGetKey(key, out JsonSpan value) && value.GetBool;
        }

        /// <summary>
        /// Try to get the bool value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetBool(string key, out bool @out) => this.TryGetBool(key.AsSpan(), out @out);

        /// <summary>
        /// Try to get the bool value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public bool TryGetBool(in ReadOnlySpan<char> key, out bool @out) {
            return this.TryGetKey(in key, out JsonSpan value) ? (@out = value.IsBool && value.GetBool) : (@out = false);
        }

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly DateTime GetDateTime => DateTime.TryParse(this.Value, out DateTime value) ? value : DateTime.MinValue;

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly DateTime AsDateTime => HasFlag((long)this.Type, (long)JsonType.DateTime) && DateTime.TryParse(this.Value, out DateTime value) ? value : DateTime.MinValue;

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public DateTime TryGetDateTime(string key) => this.TryGetDateTime(key, out DateTime value) ? value : DateTime.MinValue;

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public bool TryGetDateTime(string key, out DateTime @out) => this.TryGetDateTime(@key.AsSpan(), out @out);

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public bool TryGetDateTime(in ReadOnlySpan<char> key, out DateTime @out) {
            if (this.TryGetKey(in key, out JsonSpan value) && HasFlag((long)JsonType.DateTime, (long)this.Type)) {
                @out = value.GetDateTime;
                return true;
            }
            else {
                @out = DateTime.MinValue;
                return false;
            }
        }

    }

    #endregion

    #region ### JsonMemory ###

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public readonly struct JsonMemory : IEquatable<JsonMemory>, IComparable<JsonMemory>, IDisposable {

        /// <summary>
        /// Gets the length of the contained values for Array or Object
        /// </summary>
        public readonly int ContainerLength;
        public readonly JsonType Type;
        private readonly ReadOnlyMemory<char> Value;
        private readonly ReadOnlyMemory<char> Key;
        private readonly JsonMemory[] ContainedValues;

        private JsonMemory(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> reference, ref JsonReader reader, ref JsonMemory[] existingBuffer, int bufferIndex) {
            if (reference.IsEmpty) {
                this = Empty;
                this.Key = key;
                return;
            }
            this.Key = key;
            ushort first;
            if (reader.CurrentIndex < 0 || IsWhiteSpace(reader.CurrentValue)) {
                first = reader.AdvanceToNotWhiteSpace();
            }
            else {
                first = reader.CurrentValue;
            }
            switch (first) {
                case QUOTE: {
                    this.Type = JsonType.String;
                    this.ContainedValues = Array.Empty<JsonMemory>();
                    int left = reader.CurrentIndex + 1;
                    int right = reader.AdvanceTo(QUOTE);

                    if ((right ^ left) == 0) {
                        this.Value = ReadOnlyMemory<char>.Empty;
                    }
                    else {
                        this.Value = reference.Slice(left, right - left);
                    }
                    this.ContainerLength = 0;
                    return;
                }
                case LBRACKET: {
                    this.Type = JsonType.Array | JsonType.Disposable;
                    int left = reader.CurrentIndex;
                    if (reader.AdvanceToNotWhiteSpace() == RBRACKET) {
                        this.ContainedValues = Array.Empty<JsonMemory>();
                        this.Value = reference.Slice(left, reader.CurrentIndex - left + 1);
                        this.ContainerLength = 0;
                        return;
                    }
                    int refStart = left;

                    bool bufferSource;
                    if (existingBuffer == null) {
                        existingBuffer = JsonContainerPool.Rent(16);
                        bufferSource = true;
                    }
                    else {
                        bufferSource = false;
                    }

                    int bufPos = bufferIndex;

                    while (true) {
                        left = reader.CurrentIndex;
                        ushort leftChar = reader.CurrentValue;

                        switch (leftChar) {
                            case QUOTE: {
                                ++left;
                                reader.AdvanceTo(QUOTE);
                                int r = reader.CurrentIndex;
                                ushort advance = reader.AdvanceToNotWhiteSpace();
                                JsonContainerPool.EnsureBufferCapacity(bufPos + 1, ref existingBuffer);
                                existingBuffer[bufPos++] = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.String, reference.Slice(left, r - left));
                                if (advance == COMMA) {
                                    reader.AdvanceToNotWhiteSpace();
                                    continue;
                                }
                                else if (advance == RBRACKET) {
                                    goto ReadComplete;
                                }
                                throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                            }
                            case LBRACE:
                            case LBRACKET: {
                                int bTemp = bufPos;
                                JsonContainerPool.EnsureBufferCapacity(++bufPos, ref existingBuffer);
                                JsonMemory newValue = new JsonMemory(ReadOnlyMemory<char>.Empty, in reference, ref reader, ref existingBuffer, bufPos);
                                existingBuffer[bTemp] = newValue;
                                switch (reader.AdvanceToNotWhiteSpace()) {
                                    case RBRACKET:
                                        goto ReadComplete;
                                    case COMMA:
                                        reader.AdvanceToNotWhiteSpace();
                                        continue;
                                }
                                throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                            }
                            default: {
                                reader.AdvanceToValueEnding();
                                JsonContainerPool.EnsureBufferCapacity(bufPos + 1, ref existingBuffer);
                                ReadOnlySpan<char> data = reference.Span;
                                switch (leftChar) {
                                    case N_LOWER:
                                    case N_UPPER: {
                                        if ((reader.CurrentIndex - left) == 4) {
                                            char c = data[left + 1];
                                            if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                                                c = data[left + 2];
                                                if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                                    c = data[left + 3];
                                                    if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                                        existingBuffer[bufPos++] = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Null, NULL.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }
                                    case T_LOWER:
                                    case T_UPPER: {
                                        if ((reader.CurrentIndex - left) == 4) {
                                            char c = data[left + 1];
                                            if ((c ^ R_LOWER) == 0 || (c ^ R_UPPER) == 0) {
                                                c = data[left + 2];
                                                if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                                                    c = data[left + 3];
                                                    if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                                        existingBuffer[bufPos++] = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Boolean, bool.TrueString.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }
                                    case F_LOWER:
                                    case F_UPPER: {
                                        if ((reader.CurrentIndex - left) == 5) {
                                            char c = data[left + 1];
                                            if ((c ^ A_LOWER) == 0 || (c ^ A_UPPER) == 0) {
                                                c = data[left + 2];
                                                if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                                    c = data[left + 3];
                                                    if ((c ^ S_LOWER) == 0 || (c ^ S_UPPER) == 0) {
                                                        c = data[left + 4];
                                                        if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                                            existingBuffer[bufPos++] = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Boolean, bool.FalseString.AsMemory());
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }
                                    default: {
                                        ReadOnlyMemory<char> numberArea = reference.Slice(left, reader.CurrentIndex - left);
                                        if (IsNumber(numberArea.Span)) {
                                            existingBuffer[bufPos++] = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Number, numberArea);
                                            break;
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }
                                }

                                if (reader.CurrentValue == RBRACKET || reader.AdvanceToNotWhiteSpace() == RBRACKET) {
                                    goto ReadComplete;
                                }
                                reader.AdvanceToNotWhiteSpace();
                                continue;
                            }
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                    ReadComplete:
                    int valuesLen = bufPos - bufferIndex;
                    this.ContainerLength = valuesLen;
                    this.ContainedValues = JsonContainerPool.Rent(valuesLen);
                    existingBuffer.AsSpan(bufferIndex, valuesLen).CopyTo(this.ContainedValues.AsSpan());
                    if (bufferSource) {
                        JsonContainerPool.Return(existingBuffer);
                    }
                    this.Value = reference.Slice(refStart, reader.CurrentIndex - refStart + 1);
                    return;
                }
                case LBRACE: {
                    this.Type = JsonType.Object | JsonType.Disposable;
                    int left = reader.CurrentIndex;
                    if (reader.AdvanceToNotWhiteSpace() == RBRACE) {
                        this.ContainedValues = Array.Empty<JsonMemory>();
                        this.Value = reference.Slice(left, reader.CurrentIndex - left + 1);
                        this.ContainerLength = 0;
                        return;
                    }
                    int refStart = left;

                    bool bufferSource;
                    if (existingBuffer == null) {
                        existingBuffer = JsonContainerPool.Rent(16);
                        bufferSource = true;
                    }
                    else {
                        bufferSource = false;
                    }

                    int bufPos = bufferIndex;

                    while (true) {
                        if (reader.CurrentValue != QUOTE) {
                            reader.AdvanceTo(QUOTE);
                        }
                        int nameL = reader.CurrentIndex + 1;
                        reader.AdvanceTo(QUOTE);
                        int nameR = reader.CurrentIndex - nameL;
                        if (nameR == 1) {
                            nameL = 0;
                            nameR = 0;
                        }
                        reader.AdvanceTo(COLON);
                        reader.AdvanceToNotWhiteSpace();
                        left = reader.CurrentIndex;
                        ushort leftChar = reader.CurrentValue;

                        switch (leftChar) {
                            case QUOTE: {
                                left++;
                                reader.AdvanceTo(QUOTE);
                                int r = reader.CurrentIndex;
                                ushort advance = reader.AdvanceToNotWhiteSpace();
                                JsonContainerPool.EnsureBufferCapacity(bufPos + 1, ref existingBuffer);
                                existingBuffer[bufPos++] = new JsonMemory(reference.Slice(nameL, nameR), JsonType.String, reference.Slice(left, r - left));
                                if (advance == COMMA) {
                                    reader.Increment();
                                    goto NextObject;
                                }
                                else if (advance == RBRACE) {
                                    goto ReadComplete;
                                }
                                throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                            }
                            case LBRACE:
                            case LBRACKET: {
                                int bTemp = bufPos;
                                JsonContainerPool.EnsureBufferCapacity(++bufPos, ref existingBuffer);
                                JsonMemory newValue = new JsonMemory(reference.Slice(nameL, nameR), in reference, ref reader, ref existingBuffer, bufPos);
                                existingBuffer[bTemp] = newValue;
                                switch (reader.AdvanceToNotWhiteSpace()) {
                                    case RBRACE:
                                        goto ReadComplete;
                                    case COMMA:
                                        continue;
                                }
                                throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                            }
                            default: {
                                reader.AdvanceToValueEnding();
                                JsonContainerPool.EnsureBufferCapacity(bufPos + 1, ref existingBuffer);
                                ReadOnlySpan<char> data = reference.Span;
                                switch (leftChar) {
                                    case N_LOWER:
                                    case N_UPPER: {
                                        if ((reader.CurrentIndex - left) == 4) {
                                            char c = data[left + 1];
                                            if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                                                c = data[left + 2];
                                                if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                                    c = data[left + 3];
                                                    if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                                        existingBuffer[bufPos++] = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Null, NULL.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }
                                    case T_LOWER:
                                    case T_UPPER: {
                                        if ((reader.CurrentIndex - left) == 4) {
                                            char c = data[left + 1];
                                            if ((c ^ R_LOWER) == 0 || (c ^ R_UPPER) == 0) {
                                                c = data[left + 2];
                                                if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                                                    c = data[left + 3];
                                                    if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                                        existingBuffer[bufPos++] = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Boolean, bool.TrueString.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }
                                    case F_LOWER:
                                    case F_UPPER: {
                                        if ((reader.CurrentIndex - left) == 5) {
                                            char c = data[left + 1];
                                            if ((c ^ A_LOWER) == 0 || (c ^ A_UPPER) == 0) {
                                                c = data[left + 2];
                                                if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                                    c = data[left + 3];
                                                    if ((c ^ S_LOWER) == 0 || (c ^ S_UPPER) == 0) {
                                                        c = data[left + 4];
                                                        if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                                            existingBuffer[bufPos++] = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Boolean, bool.FalseString.AsMemory());
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }
                                    default: {
                                        ReadOnlyMemory<char> numberArea = reference.Slice(left, reader.CurrentIndex - left);
                                        if (IsNumber(numberArea.Span)) {
                                            existingBuffer[bufPos++] = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Number, numberArea);
                                            break;
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                                    }

                                }
                                if (reader.CurrentValue == RBRACE || reader.AdvanceToNotWhiteSpace() == RBRACE) {
                                    goto ReadComplete;
                                }
                                continue;
                            }
                        }
                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                        NextObject:
                        continue;
                    }

                    ReadComplete:
                    int valuesLen = bufPos - bufferIndex;
                    this.ContainerLength = valuesLen;
                    this.ContainedValues = JsonContainerPool.Rent(valuesLen);
                    existingBuffer.AsSpan(bufferIndex, valuesLen).CopyTo(this.ContainedValues.AsSpan());
                    if (bufferSource) {
                        JsonContainerPool.Return(existingBuffer);
                    }
                    this.Value = reference.Slice(refStart, reader.CurrentIndex - refStart + 1);
                    return;
                }
                case N_LOWER:
                case N_UPPER: {
                    this.ContainerLength = 0;
                    this.Type = JsonType.Null;
                    this.ContainedValues = Array.Empty<JsonMemory>();
                    this.Value = NULL.AsMemory();

                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);
                    if (right == 4) {
                        ReadOnlySpan<ushort> data = reader.Slice(left, right);
                        ushort c = data[1];
                        if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                            c = data[2];
                            if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                c = data[3];
                                if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                    return;
                                }
                            }
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                }
                case T_LOWER:
                case T_UPPER: {
                    this.ContainerLength = 0;
                    this.Type = JsonType.Boolean;
                    this.ContainedValues = Array.Empty<JsonMemory>();
                    this.Value = bool.TrueString.AsMemory();

                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);
                    if (right == 4) {
                        ReadOnlySpan<ushort> data = reader.Slice(left, right);
                        ushort c = data[1];
                        if ((c ^ R_LOWER) == 0 || (c ^ R_UPPER) == 0) {
                            c = data[2];
                            if ((c ^ U_LOWER) == 0 || (c ^ U_UPPER) == 0) {
                                c = data[3];
                                if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                    return;
                                }
                            }
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                }
                case F_LOWER:
                case F_UPPER: {
                    this.ContainerLength = 0;
                    this.Type = JsonType.Boolean;
                    this.ContainedValues = Array.Empty<JsonMemory>();
                    this.Value = bool.FalseString.AsMemory();

                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);
                    if (right == 5) {
                        ReadOnlySpan<ushort> data = reader.Slice(left, right);
                        ushort c = data[1];
                        if ((c ^ A_LOWER) == 0 || (c ^ A_UPPER) == 0) {
                            c = data[2];
                            if ((c ^ L_LOWER) == 0 || (c ^ L_UPPER) == 0) {
                                c = data[3];
                                if ((c ^ S_LOWER) == 0 || (c ^ S_UPPER) == 0) {
                                    c = data[4];
                                    if ((c ^ E_LOWER) == 0 || (c ^ E_UPPER) == 0) {
                                        return;
                                    }
                                }
                            }
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
                }
                default: {
                    this.ContainerLength = 0;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CanAdvance ? (reader.CurrentIndex - left) : (reader.CurrentIndex - left + 1);
                    this.Type = JsonType.Number;
                    this.ContainedValues = Array.Empty<JsonMemory>();

                    this.Value = reference.Slice(left, right);
                    if (IsNumber(this.Value.Span)) {
                        return;
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {this.GetStringLiteral}, {reader.ToString()})", nameof(reference));
                }
            }

            throw new ArgumentException($"Parse failed (TryParse: {reference.ToString()}, {reader.ToString()})", nameof(reference));
        }

        private JsonMemory(in JsonType type, bool rented, JsonMemory[] contents, int innerLength = -1) : this(ReadOnlyMemory<char>.Empty, in type, rented, contents, innerLength) { }

        private JsonMemory(in ReadOnlyMemory<char> key, in JsonType type, bool rented, JsonMemory[] contents, int innerLength = -1) {
            switch (type) {
                case JsonType.Array:
                case JsonType.Object:
                    this.Type = type;
                    this.ContainedValues = contents;
                    if (innerLength >= 0) {
                        this.ContainerLength = innerLength;
                    }
                    else {
                        this.ContainerLength = this.ContainedValues.Length;
                    }
                    this.Key = key;
                    this.Value = ReadOnlyMemory<char>.Empty;
                    break;
                default:
                    throw new NotSupportedException();
            }
        }

        private JsonMemory(in ReadOnlyMemory<char> key, in JsonMemory value) {
            this.Key = key;
            this.Type = value.Type;
            this.Value = value.Value;
            this.ContainerLength = value.ContainerLength;
            if (value.CanDispose) { // Dispose protection for the new value
                JsonMemory[] newPool = JsonContainerPool.Rent(value.ContainerLength);
                value.GetInsideValuesAsSpan.CopyTo(newPool.AsSpan());
                this.ContainedValues = newPool;
            }
            else {
                this.ContainedValues = value.ContainedValues;
            }
        }

        private JsonMemory(in ReadOnlyMemory<char> key, bool value) {
            this.Key = key;
            this.Type = JsonType.Boolean;
            this.Value = value ? bool.TrueString.AsMemory() : bool.FalseString.AsMemory();
            this.ContainedValues = Array.Empty<JsonMemory>();
            this.ContainerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key, double value) {
            this.Key = key;
            this.Type = JsonType.Number;
            this.Value = value.ToString().AsMemory();
            this.ContainedValues = Array.Empty<JsonMemory>();
            this.ContainerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key) {
            this.Key = key;
            this.Type = JsonType.Null;
            this.Value = NULL.AsMemory();
            this.ContainedValues = Array.Empty<JsonMemory>();
            this.ContainerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key, in string value) : this(key, value.AsMemory()) { }
        private JsonMemory(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> value) {
            this.Key = key;
            this.Type = JsonType.String;
            this.Value = value;
            this.ContainedValues = Array.Empty<JsonMemory>();
            this.ContainerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key, JsonType overridingType, in ReadOnlyMemory<char> value) {
            if (HasFlag((long)JsonType.Container, (long)overridingType)) {
                throw new ArgumentException("A container cannot be set with this override", nameof(overridingType));
            }
            this.Key = key;
            this.Type = overridingType;
            this.Value = value;
            this.ContainedValues = Array.Empty<JsonMemory>();
            this.ContainerLength = 0;
        }

        /// <summary>
        /// Default Dispose method, disposes this and all sub-objects if this was created using rented space, otherwise does nothing.
        /// </summary>
        public void Dispose() => this.Dispose(true);

        /// <summary>
        /// Disposes the current object and its sub-objects if specified.
        /// </summary>
        /// <param name="includingSubObjects">If this object will dispose everything or just itself</param>
        public void Dispose(bool includingSubObjects = true) {
            if (includingSubObjects && this.ContainerLength > 0) {
                CycleDisposeEach(in this);
            }
            if (this.CanDispose) {
                JsonContainerPool.Return(this.ContainedValues);
            }
        }

        private static void CycleDisposeEach(in JsonMemory container) {
            for (int x = 0; x < container.ContainerLength; x++) {
                container[x].Dispose(true);
            }
        }

        /// <summary>
        /// If this was created using rented space, making it eligible within using or calling dispose to save memory
        /// </summary>
        public readonly bool CanDispose => HasFlag((long)this.Type, (long)JsonType.Disposable);

        public readonly bool IsString => HasFlag((long)this.Type, (long)JsonType.String);
        public readonly bool IsBool => HasFlag((long)this.Type, (long)JsonType.Boolean);
        public readonly bool IsNumber => HasFlag((long)this.Type, (long)JsonType.Number);
        public readonly bool IsNull => HasFlag((long)this.Type, (long)JsonType.Null);
        public readonly bool IsArray => HasFlag((long)this.Type, (long)JsonType.Array);
        public readonly bool IsObject => HasFlag((long)this.Type, (long)JsonType.Object);
        public readonly bool IsValue => this.IsString || this.IsBool || this.IsNumber || this.IsNull;
        public readonly bool IsContainer => this.IsArray || this.IsObject;

        /// <summary>
        /// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
        /// </summary>
        /// <param name="key">Key or Path to desired value</param>
        /// <returns></returns>
        /// <exception cref="ArgumentException">Key was not found in object</exception>
        /// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
        public readonly ref readonly JsonMemory this[string path] => ref this[path.AsSpan()];

        /// <summary>
        /// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
        /// </summary>
        /// <param name="key">Key or Path to desired value</param>
        /// <returns></returns>
        /// <exception cref="ArgumentException">Key was not found in object</exception>
        /// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
        public readonly ref readonly JsonMemory this[ReadOnlySpan<char> key]
        {
            get
            {
                if (this.IsObject) {
                    int hash = ComputeHash(in key, out int pathLen);
                    for (int x = this.ContainerLength; --x >= 0;) {
                        ref readonly JsonMemory value = ref this.GetInsideValuesAsSpan[x];
                        if (hash == value.KeyHash) {
                            return ref value;
                        }
                        else {
                            int len = value.KeyLen;
                            if (pathLen > len && ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
                                return ref value[key.Slice(++len)];
                            }
                        }
                    }
                }

                return ref Empty;
            }
        }

        /// <summary>
        /// Get value at index of the contained NanoJson values
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        /// <exception cref="IndexOutOfRangeException"></exception>
        public readonly ref readonly JsonMemory this[int index]
        {
            get
            {
                if (this.IsContainer) {
                    return ref this.GetInsideValuesAsSpan[index];
                }
                throw new IndexOutOfRangeException();
            }
        }

        private readonly int KeyHash => ComputeHash(this.Key.Span, out _);
        private readonly int KeyLen => this.GetKeyAsSpan.Length;

        public readonly override string ToString() => this.ToString(Default_ToStringFormat);

        private readonly int GetSerializedLength(bool asValue, bool pretty, bool translateUnicode, bool lowerCaseBool, bool reparseNumbers, int indent = 0) {
            if (this.IsString) {
                return (translateUnicode ? this.GetTranslatedUnicodeLength : this.GetLength) + 2;
            }
            else if (this.IsNull) {
                return 4;
            }
            else if (this.IsNumber) {
                if (reparseNumbers) {
                    Span<char> numBuf = stackalloc char[32];
                    this.GetNumber.TryFormat(numBuf, out int refSpanLen);
                    return refSpanLen;
                }
                else {
                    return this.GetLength;
                }
            }
            else if (this.IsBool) {
                return this.GetLength;
            }
            else if (this.IsObject) {
                int length = 0;
                if (pretty && !asValue) {
                    length += indent * INDENT_LEN;
                }

                length++; // '{'
                if (this.ContainerLength == 0) {
                    if (pretty) {
                        length++; // ' '
                    }
                    length++; // '}'
                    return length;
                }

                ReadOnlySpan<JsonMemory>.Enumerator enumerator = this.GetEnumerator();
                bool moved = enumerator.MoveNext();
                if (pretty) {
                    length++; // '\n'
                    int childIndent = indent + 1;

                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        length += value.KeyLen + 4 + (childIndent * INDENT_LEN) + value.GetSerializedLength(true, pretty, translateUnicode, lowerCaseBool, reparseNumbers, childIndent);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            length += 2;
                        }
                        else {
                            length++;
                        }
                    } while (moved);

                    length += indent * INDENT_LEN; // closing indentation
                }
                else {
                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        length += value.KeyLen + 4 + value.GetSerializedLength(true, pretty, translateUnicode, lowerCaseBool, reparseNumbers, indent);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            length++;
                        }
                    } while (moved);
                }

                length++; // '}'
                return length;
            }
            else if (this.IsArray) {
                int length = 0;
                if (pretty && !asValue) {
                    length += indent * INDENT_LEN;
                }

                length++; // '['
                if (this.ContainerLength == 0) {
                    if (pretty) {
                        length++; // ' '
                    }
                    length++; // ']'
                    return length;
                }

                ReadOnlySpan<JsonMemory>.Enumerator enumerator = this.GetEnumerator();
                bool moved = enumerator.MoveNext();
                if (pretty) {
                    length++; // '\n'
                    int childIndent = indent + 1;
                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        if (value.IsValue) {
                            length += childIndent * INDENT_LEN;
                        }
                        length += value.GetSerializedLength(false, pretty, translateUnicode, lowerCaseBool, reparseNumbers, childIndent);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            length += 2;
                        }
                        else {
                            length++;
                        }
                    } while (moved);

                    length += indent * INDENT_LEN; // closing indentation
                }
                else {
                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        length += value.GetSerializedLength(false, pretty, translateUnicode, lowerCaseBool, reparseNumbers, indent);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            length++; // ','
                        }
                    } while (moved);
                }

                length++; // ']'
                return length;
            }
            else {
                return 0;
            }
        }

        public readonly string ToString(ToStringFormat format) {
            bool pretty = HasFlag((long)format, (long)ToStringFormat.Pretty);
            bool translateUnicode = HasFlag((long)format, (long)ToStringFormat.TranslateUnicode);
            bool lowerCaseBool = HasFlag((long)format, (long)ToStringFormat.LowerCaseBool);
            bool reparseNumbers = HasFlag((long)format, (long)ToStringFormat.ReParseNumbers);

            if (this.IsString) {
                if (translateUnicode) {
                    return this.GetStringDecoded;
                }
                else {
                    return this.GetStringLiteral;
                }
            }
            else if (this.IsNull) {
                if (HasFlag((long)format, (long)ToStringFormat.NullReturnsEmptyReference)) {
                    return null;
                }
                else {
                    return NULL;
                }
            }
            else if (this.IsNumber) {
                if (reparseNumbers) {
                    return this.GetNumberString;
                }
                else {
                    return this.GetStringLiteral;
                }
            }
            else if (this.IsBool) {
                if (MemoryMarshal.GetReference(this.Value.Span) == 'T') {
                    return lowerCaseBool ? TRUE_l : TRUE_u;
                }
                else {
                    return lowerCaseBool ? FALSE_l : FALSE_u;
                }
            }
            int exactCapacity = Math.Max(1, this.GetSerializedLength(false, pretty, translateUnicode, lowerCaseBool, reparseNumbers));
            return string.Create(exactCapacity, (this, pretty, translateUnicode, lowerCaseBool, reparseNumbers, INDENT_TABS), (span, state) => {
                int indent = 0;
                int sbPos = 0;
                state.Item1.ProcessString(false, state.pretty, state.translateUnicode, state.lowerCaseBool, state.reparseNumbers, span, ref indent, ref sbPos, state.INDENT_TABS.AsSpan());
            });
        }

        /// <summary>
        /// Recursive method to build the json ToString output
        /// </summary>
        private readonly void ProcessString(bool asValue, bool pretty, bool translateUnicode, bool lowerCaseBool, bool reparseNumbers, in Span<char> sb, ref int indent, ref int sbPos, in ReadOnlySpan<char> indentSpan) {
            if (this.IsString) {
                sb[sbPos++] = '"';
                if (translateUnicode) {
                    TranslateUnicodeIntoBufferFromSpan(this.GetValueAsSpan, sb, ref sbPos);
                }
                else {
                    this.GetValueAsSpan.CopyTo(sb.Slice(sbPos, this.GetLength));
                    sbPos += this.GetLength;
                }
                sb[sbPos++] = '"';
                return;
            }
            else if (this.IsNull) {
                this.GetValueAsSpan.CopyTo(sb.Slice(sbPos, 4));
                sbPos += 4;
                return;
            }
            else if (this.IsNumber) {
                if (reparseNumbers) {
                    this.GetNumber.TryFormat(sb.Slice(sbPos), out int refSpanLen);
                    sbPos += refSpanLen;
                }
                else {
                    this.GetValueAsSpan.CopyTo(sb.Slice(sbPos, this.GetLength));
                    sbPos += this.GetLength;
                }
                return;
            }
            else if (this.IsBool) {
                this.GetValueAsSpan.CopyTo(sb.Slice(sbPos, this.GetLength));
                if (lowerCaseBool) {
                    switch (MemoryMarshal.GetReference(this.GetValueAsSpan)) {
                        case 'F':
                            sb[sbPos] = 'f';
                            break;
                        case 'T':
                            sb[sbPos] = 't';
                            break;
                    }
                }
                sbPos += this.GetLength;
            }
            else if (this.IsObject) {
                int x;
                int y;
                if (pretty && !asValue) {
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }

                sb[sbPos++] = '{';

                ReadOnlySpan<JsonMemory>.Enumerator enumerator = this.GetEnumerator();
                bool moved = enumerator.MoveNext();
                if (this.ContainerLength == 0 || !moved) {
                    if (pretty) {
                        sb[sbPos++] = ' ';
                    }
                    sb[sbPos++] = '}';
                    return;
                }

                if (pretty) {
                    indent++;
                    sb[sbPos++] = '\n';

                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        for (y = indent; --y >= 0;) {
                            indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                        sb[sbPos++] = '"';
                        value.GetKeyAsSpan.CopyTo(sb.Slice(sbPos, value.KeyLen));
                        sbPos += value.KeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        value.ProcessString(true, pretty, translateUnicode, lowerCaseBool, reparseNumbers, sb, ref indent, ref sbPos, in indentSpan);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                        sb[sbPos++] = '\n';
                    } while (moved);

                    indent--;
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }
                else {
                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        sb[sbPos++] = '"';
                        value.GetKeyAsSpan.CopyTo(sb.Slice(sbPos, value.KeyLen));
                        sbPos += value.KeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        value.ProcessString(true, pretty, translateUnicode, lowerCaseBool, reparseNumbers, in sb, ref indent, ref sbPos, in indentSpan);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                    } while (moved);
                }

                sb[sbPos++] = '}';
                return;
            }
            else if (this.IsArray) {
                int x;
                int y;
                if (pretty && !asValue) {
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }
                sb[sbPos++] = '[';

                ReadOnlySpan<JsonMemory>.Enumerator enumerator = this.GetEnumerator();
                bool moved = enumerator.MoveNext();
                if (this.ContainerLength == 0 || !moved) {
                    if (pretty) {
                        sb[sbPos++] = ' ';
                    }
                    sb[sbPos++] = ']';
                    return;
                }

                if (pretty) {
                    indent++;
                    sb[sbPos++] = '\n';

                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        if (value.IsValue) {
                            for (y = indent; --y >= 0;) {
                                indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                                sbPos += INDENT_LEN;
                            }
                        }
                        value.ProcessString(false, pretty, translateUnicode, lowerCaseBool, reparseNumbers, in sb, ref indent, ref sbPos, in indentSpan);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                        sb[sbPos++] = '\n';
                    } while (moved);

                    indent--;
                    for (x = indent; --x >= 0;) {
                        indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
                        sbPos += INDENT_LEN;
                    }
                }
                else {
                    do {
                        ref readonly JsonMemory value = ref enumerator.Current;
                        value.ProcessString(false, pretty, translateUnicode, lowerCaseBool, reparseNumbers, in sb, ref indent, ref sbPos, in indentSpan);
                        moved = enumerator.MoveNext();
                        if (moved) {
                            sb[sbPos++] = ',';
                        }
                    } while (moved);
                }

                sb[sbPos++] = ']';
                return;
            }
        }

        /// <summary>
        /// Get the length of the reference area or if no reference is available, the combined internal GetLength values
        /// </summary>
        public readonly int GetLength
        {
            get
            {
                if (this.Value.IsEmpty) {
                    int len = 0;
                    for (int x = this.ContainerLength; --x >= 0;) {
                        len += this.GetInsideValuesAsSpan[x].GetLength;
                    }
                    return len;
                }
                else {
                    return this.Value.Length;
                }
            }
        }

        /// <summary>
        /// Get the literal string value of the object
        /// </summary>
        public readonly string GetStringLiteral => this.Value.ToString();

        public readonly int GetTranslatedUnicodeLength => GetStringDecodeLengthFromSpan(this.GetValueAsSpan);

        /// <summary>
        /// Get the decoded string value of the object
        /// </summary>
        public readonly string GetStringDecoded => GetStringDecodedFromSpan(this.GetValueAsSpan);

        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly string TryGetString(string key, bool decoded = true) => this.TryGetString(key.AsSpan(), decoded);
        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetString(string key, out string @out, bool decoded = true) => this.TryGetString(key.AsSpan(), out @out, decoded);

        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly string TryGetString(in ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(key, out JsonMemory value) && value.IsString ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetString(in ReadOnlySpan<char> key, out string @out, bool decoded = true) {
            if (this.TryGetKey(key, out JsonMemory value) && value.IsString) {
                @out = decoded ? value.GetStringDecoded : value.GetStringLiteral;
                return true;
            }
            else {
                @out = string.Empty;
                return false;
            }
        }

        /// <summary>
        /// Get the data used inside This object
        /// </summary>
        public readonly ReadOnlySpan<char> GetValueAsSpan => this.Value.Span;

        /// <summary>
        /// Get the data used inside This object
        /// </summary>
        public readonly ReadOnlyMemory<char> GetValueAsMemory => this.Value;

        /// <summary>
        /// Get the number contained inside This object as a <c>double</c>
        /// </summary>
        public readonly double GetNumber
        {
            get
            {
                double returnNumber = double.NaN;
                if (this.IsNumber) {
                    returnNumber = ParseNumber(this.GetValueAsSpan);
                }
                return returnNumber;
            }
        }

        /// <summary>
        /// Get the number contained inside This object and parse it tostring
        /// </summary>
        private readonly string GetNumberString => this.GetNumber.ToString();

        /// <summary>
        /// Get the number contained inside This object
        /// </summary>
        public readonly T GetNumberOfType<T>() where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            return double.TryParse(this.GetValueAsSpan, out double value) ? GetConvertedValue<T>(value) : default;
        }


        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly double TryGetNumber(string key) => this.TryGetNumber(key.AsSpan());
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetNumber(string key, out double @out) => this.TryGetNumber(@key.AsSpan(), out @out);

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly double TryGetNumber(in ReadOnlySpan<char> key) => this.TryGetKey(in key, out JsonMemory value) ? value.GetNumber : double.NaN;
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetNumber(in ReadOnlySpan<char> key, out double @out) {
            if (this.TryGetKey(in key, out JsonMemory value) && value.IsNumber) {
                return double.TryParse(value.GetValueAsSpan, out @out);
            }
            else {
                @out = double.NaN;
                return false;
            }
        }

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly T TryGetNumber<T>(string key) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            return this.TryGetNumber<T>(key.AsSpan());
        }
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetNumber<T>(string key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            return this.TryGetNumber(key.AsSpan(), out @out);
        }

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly T TryGetNumber<T>(in ReadOnlySpan<char> key) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            this.TryGetNumber(in key, out T value);
            return value;
        }

        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetNumber<T>(in ReadOnlySpan<char> key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            if (this.TryGetKey(in key, out JsonMemory value) && value.IsNumber) {
                @out = value.GetNumberOfType<T>();
                return true;
            }
            else {
                @out = default;
                return false;
            }
        }

        /// <summary>
        /// Get the values contained inside This object but as a new array
        /// </summary>
        public readonly JsonMemory[] GetCopyOfInsideValues
        {
            get
            {
                if (this.ContainerLength == 0) {
                    return Array.Empty<JsonMemory>();
                }
                else {
                    return this.GetInsideValuesAsSpan.ToArray();
                }
            }
        }

        /// <summary>
        /// Get the values contained inside This object
        /// </summary>
        public readonly ReadOnlySpan<JsonMemory> GetInsideValuesAsSpan => (ReadOnlySpan<JsonMemory>)this.ContainedValues.AsSpan(0, this.ContainerLength);

        /// <summary>
        /// Get the values contained inside This object
        /// </summary>
        public readonly ReadOnlyMemory<JsonMemory> GetInsideValuesAsMemory => (ReadOnlyMemory<JsonMemory>)this.ContainedValues.AsMemory(0, this.ContainerLength);

        /// <summary>
        /// Get the bool value of This object
        /// </summary>
        public readonly bool GetBool => bool.TryParse(this.GetValueAsSpan, out bool value) && value;

        /// <summary>
        /// Get the inner values of this Object/Array as a string array
        /// </summary>
        public readonly string[] ToStringArrayDecoded
        {
            get
            {
                string[] array = new string[this.ContainerLength];
                for (int x = 0; x < this.ContainerLength; x++) {
                    array[x] = this[x].GetStringDecoded;
                }
                return array;
            }
        }

        /// <summary>
        /// Get the inner values of this Object/Array as a string array
        /// </summary>
        public readonly string[] ToStringArray
        {
            get
            {
                string[] array = new string[this.ContainerLength];
                for (int x = 0; x < this.ContainerLength; x++) {
                    array[x] = this[x].GetStringLiteral;
                }
                return array;
            }
        }

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly DateTime GetDateTime => DateTime.Parse(this.GetValueAsSpan);

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly DateTime TryGetDateTime(string key) => this.TryGetKey(key, out JsonMemory value) ? value.GetDateTime : DateTime.MinValue;

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly bool TryGetDateTime(string key, out DateTime @out) => this.TryGetDateTime(key.AsSpan(), out @out);

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly bool TryGetDateTime(in ReadOnlySpan<char> key, out DateTime @out) {
            if (this.TryGetKey(in key, out JsonMemory value) && HasFlag((long)JsonType.DateTime, (long)value.Type)) {
                @out = value.GetDateTime;
                return true;
            }
            else {
                @out = DateTime.MinValue;
                return false;
            }
        }

        /// <summary>
        /// Try to get the bool value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetBool(string key) => this.GetKeyOrEmpty(key.AsSpan()).GetBool;

        /// <summary>
        /// Try to get the bool value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetBool(string key, out bool @out) => this.TryGetBool(key.AsSpan(), out @out);

        /// <summary>
        /// Try to get the bool value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetBool(in ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(in key, out JsonMemory value) ? (@out = value.IsBool && value.GetBool) : (@out = false);

        /// <summary>
        /// Get the key of This object
        /// </summary>
        public readonly string GetKey => this.Key.IsEmpty ? string.Empty : this.Key.ToString();

        /// <summary>
        /// Get the key of This object
        /// </summary>
        public readonly ReadOnlySpan<char> GetKeyAsSpan => this.Key.IsEmpty ? ReadOnlySpan<char>.Empty : this.Key.Span;

        /// <summary>
        /// Get the key of This object
        /// </summary>
        public readonly ReadOnlyMemory<char> GetKeyAsMemory => this.Key.IsEmpty ? ReadOnlyMemory<char>.Empty : this.Key;

        /// <summary>
        /// Compare the key of This object
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool CompareKey(string key) => this.CompareKey(key.AsSpan());
        /// <summary>
        /// Compare the key of This object
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool CompareKey(in ReadOnlySpan<char> key) {
            return this.KeyHash == ComputeHash(key, out _);
        }

        /// <summary>
        /// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
        /// </summary>
        /// <param name="key"></param>
        /// <returns>Key Found</returns>
        public readonly bool TryGetKey(string key, out JsonMemory found) => this.TryGetKey(key.AsSpan(), out found);
        /// <summary>
        /// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
        /// </summary>
        /// <param name="key"></param>
        /// <returns>Key Found</returns>
        public readonly bool TryGetKey(in ReadOnlySpan<char> key, out JsonMemory found) {
            if (this.IsObject) {
                int hash = ComputeHash(in key, out int pathLen);
                for (int x = this.ContainerLength; --x >= 0;) {
                    ref readonly JsonMemory value = ref this.GetInsideValuesAsSpan[x];
                    if (hash == value.KeyHash) {
                        found = value;
                        return true;
                    }
                    else {
                        int len = value.KeyLen;
                        if (pathLen > len && ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
                            if (value.TryGetKey(key[++len..], out found)) {
                                return true;
                            }
                        }
                    }
                }
            }
            found = Empty;
            return false;
        }

        /// <summary>
        /// <c>ref readonly</c> version of TryGetKey, returns the value as a reference with the success value in the out
        /// </summary>
        /// <param name="key"></param>
        /// <returns>Key reference</returns>
        public readonly ref readonly JsonMemory TryGetKeyRef(string key, out bool found) => ref this.TryGetKeyRef(key.AsSpan(), out found);

        /// <summary>
        /// <c>ref readonly</c> version of TryGetKey, returns the value as a reference with the success value in the out
        /// </summary>
        /// <param name="key"></param>
        /// <returns>Key reference</returns>
        public readonly ref readonly JsonMemory TryGetKeyRef(ReadOnlySpan<char> key, out bool found) {
            if (this.IsObject) {
                int hash = ComputeHash(in key, out int pathLen);
                for (int x = this.ContainerLength; --x >= 0;) {
                    ref readonly JsonMemory value = ref this.GetInsideValuesAsSpan[x];
                    if (hash == value.KeyHash) {
                        found = true;
                        return ref value;
                    }
                    int len = value.KeyLen;
                    if (pathLen > len && ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
                        ref readonly JsonMemory candidate = ref value.TryGetKeyRef(key.Slice(++len), out found);
                        if (found) {
                            return ref candidate;
                        }
                    }
                }
            }
            found = false;
            return ref Empty;
        }

        /// <summary>
        /// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
        /// </summary>
        public readonly JsonMemory GetKeyOrEmpty(string key) => this.GetKeyOrEmpty(key.AsSpan());
        /// <summary>
        /// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
        /// </summary>
        public readonly JsonMemory GetKeyOrEmpty(in ReadOnlySpan<char> key) => this.TryGetKey(in key, out JsonMemory found) ? found : Empty;

        public readonly ReadOnlySpan<JsonMemory>.Enumerator GetEnumerator() => this.GetInsideValuesAsSpan.GetEnumerator();

        public readonly override bool Equals(object obj) => obj is JsonMemory other && this.Equals(other);
        public readonly bool Equals(JsonMemory other) {
            return this.Type.Equals(other.Type)
              && this.ContainedValues.Equals(other.ContainedValues)
              && this.KeyHash == other.KeyHash
              && this.GetValueAsSpan.Equals(other.GetValueAsSpan, StringComparison.Ordinal)
              && this.ContainerLength == other.ContainerLength;
        }
        public static bool operator ==(in JsonMemory left, in JsonMemory right) {
            return left.Equals(right);
        }

        public static bool operator !=(in JsonMemory left, in JsonMemory right) {
            return !(left == right);
        }

        public readonly override int GetHashCode() {
            return HashCode.Combine(this.Type, this.ContainedValues, this.Value, this.KeyHash, this.ContainerLength);
        }

        public static implicit operator JsonMemory(in JsonSpan span) {
            return Pin(in span);
        }
        public static implicit operator JsonSpan(in JsonMemory self) {
            if (self.Key.IsEmpty) {
                return new JsonSpan(self.GetValueAsSpan);
            }
            else {
                return new JsonSpan(self.GetKeyAsSpan, self.GetValueAsSpan);
            }
        }

        public static implicit operator JsonMemory[](in JsonMemory container) {
            return container.GetCopyOfInsideValues;
        }

        public int CompareTo(JsonMemory other) {
            return this.KeyHash.CompareTo(other.KeyHash);
        }

        private readonly static JsonMemory Empty_Body = new JsonMemory(ReadOnlyMemory<char>.Empty);
        public static ref readonly JsonMemory Empty => ref Empty_Body;

        public static JsonMemory ParseJson(string key, string data) {
            return ParseJson(key.AsMemory(), data.AsMemory());
        }

        private static JsonMemory ParseJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> data) {
            if (key.Span.Trim().IsEmpty) {
                if (data.Span.Trim().IsEmpty) {
                    return Empty;
                }
                return ParseJson(in data);
            }
            else {
                if (data.Span.Trim().IsEmpty) {
                    return CreateNull(key);
                }
                JsonMemory[] buffer = null;
                JsonReader reader = new JsonReader(data.Span);
                JsonMemory parsed = new JsonMemory(in key, in data, ref reader, ref buffer, 0);
                return parsed;
            }
        }

        public static bool TryParseJson(string key, string data, out JsonMemory parsed) {
            return TryParseJson(key.AsMemory(), data.AsMemory(), out parsed);
        }

        private static bool TryParseJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> data, out JsonMemory parsed) {
            try {
                if (key.IsEmpty) {
                    parsed = ParseJson(in data);
                }
                else {
                    parsed = ParseJson(in key, in data);
                }
                if (parsed.IsNull) {
                    return false;
                }
                return true;
            }
            catch {
                parsed = Empty;
                return false;
            }
        }

        public static JsonMemory ParseJson(string data) {
            return ParseJson(data.AsMemory());
        }

        private static JsonMemory ParseJson(in ReadOnlyMemory<char> data) {
            if (data.Span.Trim().IsEmpty) {
                return Empty;
            }
            JsonMemory[] buffer = null;
            JsonReader reader = new JsonReader(data.Span);
            JsonMemory parsed = new JsonMemory(ReadOnlyMemory<char>.Empty, in data, ref reader, ref buffer, 0);
            return parsed;
        }

        public static bool TryParseJson(string data, out JsonMemory parsed) {
            return TryParseJson(data.AsMemory(), out parsed);
        }

        private static bool TryParseJson(in ReadOnlyMemory<char> data, out JsonMemory parsed) {
            try {
                parsed = ParseJson(in data);
                if (parsed.IsNull) {
                    return false;
                }
                return true;
            }
            catch {
                parsed = Empty;
                return false;
            }
        }

        public static JsonMemory Pin(in JsonSpan data) {
            if (data.IsNothing) {
                return Empty;
            }
            switch (data.Type) {
                case JsonType.Null:
                    if (data.Key.IsEmpty) {
                        return Empty;
                    }
                    else {
                        return new JsonMemory(data.Key.ToArray());
                    }
                case JsonType.Object:
                case JsonType.Array: // JsonSpan is on demand so this information hasnt been processed so translate as if fresh
                    return ParseJson(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, data.Value.ToArray().AsMemory());
                case JsonType.String:
                    return CreateString(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, data.Value.ToString());
                case JsonType.Number:
                    return CreateNumber(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, double.Parse(data.Value));
                case JsonType.Boolean:
                    return CreateBool(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, bool.Parse(data.Value));
                default:
                    throw new NotSupportedException();
            }
        }

        /// <summary>
        /// Remakes the existing JsonMemory object, usually an internal node, by allocating the segments to arrays. Typically needed when you want to deallocate a large Json string container but keeping its smaller internal nodes
        /// </summary>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <exception cref="NotSupportedException"></exception>
        public static JsonMemory Pin(in JsonMemory data) {
            switch (data.Type) {
                case JsonType.Null:
                    if (data.Key.IsEmpty) {
                        return Empty;
                    }
                    else {
                        return new JsonMemory(data.Key.ToArray());
                    }
                case JsonType.Object:
                case JsonType.Array: // Inner bodies need re-parsing as the originals reference the same allocated memory and we want it to point to a new area
                    return ParseJson(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, data.Value.ToArray().AsMemory());
                case JsonType.String:
                    return CreateString(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, data.Value.ToString());
                case JsonType.Number:
                    return CreateNumber(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, double.Parse(data.GetValueAsSpan));
                case JsonType.Boolean:
                    return CreateBool(data.Key.IsEmpty ? data.Key.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, bool.Parse(data.GetValueAsSpan));
                default:
                    throw new NotSupportedException();
            }
        }

        public static JsonMemory CreateArray(string key, JsonMemory[] data, bool AllocateNewContainer = false) {
            return CreateArray(key.AsMemory(), AllocateNewContainer ? (JsonMemory[])data.Clone() : data);
        }

        /// <param name="AllocateNewContainer">If a container should be rented and become eligible for Dispose</param>
        /// <param name="lengthRelevant">From 0, the length of elements relevent to the Json</param>
        private static JsonMemory CreateArray(in ReadOnlyMemory<char> key, JsonMemory[] data, bool AllocateNewContainer = false, int lengthRelevant = -1) {
            JsonMemory[] space;
            if (AllocateNewContainer) {
                space = JsonContainerPool.Rent(data.Length);
                data.AsSpan().CopyTo(space.AsSpan());
            }
            else {
                space = data;
            }
            if (lengthRelevant < 0) {
                lengthRelevant = space.Length;
            }

            return new JsonMemory(in key, JsonType.Array, AllocateNewContainer, space, lengthRelevant);
        }

        /// <param name="AllocateNewContainer">If a container should be rented and become eligible for Dispose</param>
        /// <param name="lengthRelevant">From 0, the length of elements relevent to the Json</param>
        public static JsonMemory CreateArray(JsonMemory[] data, bool AllocateNewContainer = false, int lengthRelevant = -1) {
            JsonMemory[] space;
            if (AllocateNewContainer) {
                space = JsonContainerPool.Rent(data.Length);
                data.AsSpan().CopyTo(space.AsSpan());
            }
            else {
                space = data;
            }
            if (lengthRelevant < 0) {
                lengthRelevant = space.Length;
            }

            return new JsonMemory(JsonType.Array, AllocateNewContainer, space, lengthRelevant);
        }

        public static JsonMemory CreateObject(string key, JsonMemory[] data, bool AllocateNewContainer = false) {
            return CreateObject(key.AsMemory(), data, AllocateNewContainer);
        }

        /// <param name="AllocateNewContainer">If a container should be rented and become eligible for Dispose</param>
        /// <param name="lengthRelevant">From 0, the length of elements relevent to the Json</param>
        private static JsonMemory CreateObject(in ReadOnlyMemory<char> key, JsonMemory[] data, bool AllocateNewContainer = false, int lengthRelevant = -1) {
            JsonMemory[] space;
            if (AllocateNewContainer) {
                space = JsonContainerPool.Rent(data.Length);
                data.AsSpan().CopyTo(space.AsSpan());
            }
            else {
                space = data;
            }
            if (lengthRelevant < 0) {
                lengthRelevant = space.Length;
            }

            return new JsonMemory(in key, JsonType.Object, AllocateNewContainer, space, lengthRelevant);
        }

        /// <param name="AllocateNewContainer">If a container should be rented and become eligible for Dispose</param>
        /// <param name="lengthRelevant">From 0, the length of elements relevent to the Json</param>
        public static JsonMemory CreateObject(JsonMemory[] data, bool AllocateNewContainer = false, int lengthRelevant = -1) {
            JsonMemory[] space;
            if (AllocateNewContainer) {
                space = JsonContainerPool.Rent(data.Length);
                data.AsSpan().CopyTo(space.AsSpan());
            }
            else {
                space = data;
            }
            if (lengthRelevant < 0) {
                lengthRelevant = space.Length;
            }

            return new JsonMemory(JsonType.Object, AllocateNewContainer, space, lengthRelevant);
        }

        public static JsonMemory CreateString(string data) {
            return CreateString(ReadOnlyMemory<char>.Empty, data);
        }

        public static JsonMemory CreateString(string key, string data) {
            return CreateString(key.AsMemory(), data);
        }

        private static JsonMemory CreateString(in ReadOnlyMemory<char> key, string data) {
            return new JsonMemory(in key, in data);
        }

        public static JsonMemory CreateDateTime(in DateTime data) {
            return CreateDateTime(ReadOnlyMemory<char>.Empty, in data);
        }

        public static JsonMemory CreateDateTime(string key, in DateTime data) {
            return CreateDateTime(key.AsMemory(), in data);
        }

        private static JsonMemory CreateDateTime(in ReadOnlyMemory<char> key, in DateTime data) {
            return new JsonMemory(in key, data.ToString("o"));
        }

        /// <summary>
        /// Take an existing value and assign its values to a new object with a new key
        /// </summary>
        /// <param name="key"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        public static JsonMemory AssignKeyToValue(string key, in JsonMemory data) {
            return AssignKeyToValue(key.AsMemory(), in data);
        }

        /// <summary>
        /// Take an existing value and assign its values to a new object with a new key
        /// </summary>
        /// <param name="key"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        private static JsonMemory AssignKeyToValue(in ReadOnlyMemory<char> key, in JsonMemory data) {
            return new JsonMemory(in key, in data);
        }

        public static JsonMemory CreateBool(bool data) {
            return CreateBool(ReadOnlyMemory<char>.Empty, data);
        }

        public static JsonMemory CreateBool(string key, bool data) {
            return CreateBool(key.AsMemory(), data);
        }

        private static JsonMemory CreateBool(in ReadOnlyMemory<char> key, bool data) {
            return new JsonMemory(in key, data);
        }

        public static JsonMemory CreateNumber(double data) {
            return CreateNumber(ReadOnlyMemory<char>.Empty, data);
        }

        public static JsonMemory CreateNumber(string key, double data) {
            return CreateNumber(key.AsMemory(), data);
        }

        private static JsonMemory CreateNumber(in ReadOnlyMemory<char> key, double data) {
            return new JsonMemory(in key, data);
        }

        public static ref readonly JsonMemory CreateNull() {
            return ref Empty;
        }

        public static JsonMemory CreateNull(string key) {
            return CreateNull(key.AsMemory());
        }

        private static JsonMemory CreateNull(in ReadOnlyMemory<char> key) {
            return new JsonMemory(in key);
        }

    }

    #endregion

    #region ### JsonReader ###

    [StructLayout(LayoutKind.Sequential)]
    public ref struct JsonReader {

        public static JsonReader Empty => new JsonReader();

        private static int _toStringSegmentLength = 20;
        /// <summary>
        /// Change this value to alter the output length of ToString
        /// </summary>
        public static int ToStringSegmentLength
        {
            get => _toStringSegmentLength;
            set
            {
                if (value < 1) {
                    _toStringSegmentLength = 1;
                }
                else {
                    _toStringSegmentLength = value;
                }
            }
        }

        public int CurrentIndex { get; private set; }

        private readonly int endIndex;

        private readonly ReadOnlySpan<ushort> source;

        private Vector<ushort> toCompare;
        private Span<ushort> GetToCompareSpan => MemoryMarshal.Cast<Vector<ushort>, ushort>(MemoryMarshal.CreateSpan(ref this.toCompare, 1));

        public readonly ref readonly ushort CurrentValue => ref this.source[this.CurrentIndex];
        public readonly char CurrentChar => (char)this.CurrentValue;

        public readonly bool CanAdvance => this.CurrentIndex < this.endIndex;
        public readonly bool CanRetreat => this.CurrentIndex > 0;

        private readonly int Segment => Vector<ushort>.Count;
        private readonly int SegmentZeroBased => this.Segment - 1;

        public JsonReader(in ReadOnlySpan<char> data, bool fromEnd = false) {
            this.source = MemoryMarshal.Cast<char, ushort>(data);
            this.endIndex = data.Length - 1;
            this.toCompare = new Vector<ushort>();

            if (fromEnd) {
                this.CurrentIndex = data.Length;
            }
            else {
                this.CurrentIndex = -1;
            }
        }

        /// <summary>
        /// To output the current information about the reader for debug purposes
        /// </summary>
        /// <returns></returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override readonly string ToString() {
            return $"Reader: {{Source ..{ToStringSegmentLength}:: {(this.CurrentIndex < 0 || this.CurrentIndex > this.endIndex ? (this.CurrentIndex < 0 ? "[Read index at Start]" : "[Read index at End]") : MemoryMarshal.Cast<ushort, char>(this.source[Math.Max(0, this.CurrentIndex - ToStringSegmentLength)..(this.CurrentIndex + 1)]).ToString())}, Pos: {this.CurrentIndex}, Char: '{(this.CurrentIndex < 0 || this.CurrentIndex > this.endIndex ? '\0' : this.CurrentChar)}'}}";
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public readonly ReadOnlySpan<ushort> Slice(int index, int len) {
            return this.source.Slice(index, len);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public readonly ReadOnlySpan<char> SliceCast(int index, int len) {
            return MemoryMarshal.Cast<ushort, char>(this.Slice(index, len));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Increment() {
            if (this.CanAdvance) {
                this.CurrentIndex++;
                return true;
            }
            else {
                return false;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Decrement() {
            if (this.CanRetreat) {
                this.CurrentIndex--;
                return true;
            }
            else {
                return false;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SetIndexPosition(int index) {
            if (index < 0) {
                this.SetIndexToStart();
            }
            else if (index > this.endIndex) {
                this.SetIndexToEnd();
            }
            else {
                this.CurrentIndex = index;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SetIndexToStart() {
            this.CurrentIndex = -1;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SetIndexToEnd() {
            this.CurrentIndex = this.endIndex + 1;
        }

        #region # ADVANCE #

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort Advance(int amount = 1) {
            while (this.Increment() && --amount > 0) { }
            return this.CurrentValue;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort Advance() {
            this.Increment();
            return this.CurrentValue;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int AdvanceTo(ushort search) {
            if (!this.Increment()) {
                return -1;
            }
            int segmentMinus = this.SegmentZeroBased;
            Vector<ushort> searchVector = new Vector<ushort>(search);
            Span<ushort> buf = this.GetToCompareSpan;
            do {
                if (this.CurrentIndex + segmentMinus > this.endIndex) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus;
                    continue;
                }
                for (int x = 0; x <= segmentMinus; x++) {
                    if (eq[x] > 0) {
                        this.CurrentIndex += x;
                        return this.CurrentIndex;
                    }
                }
                this.CurrentIndex += segmentMinus;
            } while (this.Increment());
            return -1;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int AdvanceToNot(ushort search) {
            if (!this.Increment()) {
                return -1;
            }
            int segmentMinus = this.SegmentZeroBased;
            Vector<ushort> searchVectors = new Vector<ushort>(search);
            Span<ushort> buf = this.GetToCompareSpan;
            do {
                if (this.CurrentIndex + segmentMinus > this.endIndex) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, searchVectors);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentIndex;
                }
                for (int x = 0; x <= segmentMinus; x++) {
                    if (eq[x] == 0) {
                        this.CurrentIndex += x;
                        return this.CurrentIndex;
                    }
                }
                this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
            } while (this.Increment());
            return -1;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort AdvanceTo(in ReadOnlySpan<ushort> search) {
            if (!this.Increment()) {
                return '\0';
            }
            int segmentMinus = this.SegmentZeroBased;
            int searchLen = search.Length;
            Span<ushort> buf = this.GetToCompareSpan;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[searchLen];
            for (int i = 0; i < searchLen; i++) {
                searchVectors[i] = new Vector<ushort>(search[i]);
            }
            do {
                if (this.CurrentIndex + segmentMinus > this.endIndex) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, MemoryMarshal.GetReference(searchVectors));
                for (int x = 1; x < searchLen; x++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(this.toCompare, searchVectors[x]));
                }
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
                for (int x = 0; x <= segmentMinus; x++) {
                    if (eq[x] > 0) {
                        this.CurrentIndex += x;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
            } while (this.Increment());
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort AdvanceToNot(in ReadOnlySpan<ushort> search) {
            if (!this.Increment()) {
                return '\0';
            }
            int segmentMinus = this.SegmentZeroBased;
            int searchLen = search.Length;
            Span<ushort> buf = this.GetToCompareSpan;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[searchLen];
            for (int i = 0; i < searchLen; i++) {
                searchVectors[i] = new Vector<ushort>(search[i]);
            }
            do {
                if (this.CurrentIndex + segmentMinus > this.endIndex) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, MemoryMarshal.GetReference(searchVectors));
                for (int x = 1; x < searchLen; x++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(this.toCompare, searchVectors[x]));
                }
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                for (int x = 0; x <= segmentMinus; x++) {
                    if (eq[x] == 0) {
                        this.CurrentIndex += x;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
            } while (this.Increment())
                ;
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort AdvanceToWhiteSpace() {
            if (!this.Increment()) {
                return '\0';
            }
            int segmentMinus = this.SegmentZeroBased;
            Vector<ushort> searchVector = new Vector<ushort>(33); // 32 == ' '
            Span<ushort> buf = this.GetToCompareSpan;
            do {
                if (this.CurrentIndex + segmentMinus > this.endIndex) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.LessThan(this.toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
                for (int x = 0; x <= segmentMinus; x++) {
                    if (eq[x] > 0 && IsWhiteSpace(buf[x])) {
                        this.CurrentIndex += x;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
            } while (this.Increment());
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort AdvanceToNotWhiteSpace() {
            if (!this.Increment()) {
                return '\0';
            }
            int segmentMinus = this.SegmentZeroBased;
            Vector<ushort> searchVector = new Vector<ushort>(32); // 32 == ' '
            Span<ushort> buf = this.GetToCompareSpan;
            do {
                if (this.CurrentIndex + segmentMinus > this.endIndex) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.GreaterThan(this.toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
                for (int x = 0; x <= segmentMinus; x++) {
                    if (eq[x] > 0 && !IsWhiteSpace(buf[x])) {
                        this.CurrentIndex += x;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
            } while (this.Increment());
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort AdvanceToValueEnding() {
            if (!this.Increment()) {
                return '\0';
            }
            int segmentMinus = this.SegmentZeroBased;
            Vector<ushort> commaVector = new Vector<ushort>(COMMA);
            Vector<ushort> rbraceVector = new Vector<ushort>(RBRACE);
            Vector<ushort> rbracketVector = new Vector<ushort>(RBRACKET);
            Vector<ushort> whitespace = new Vector<ushort>(33);
            Span<ushort> buf = this.GetToCompareSpan;
            do {
                if (this.CurrentIndex + segmentMinus > this.endIndex) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.BitwiseOr(
                    Vector.LessThan(this.toCompare, whitespace),
                    Vector.BitwiseOr(
                        Vector.BitwiseOr(
                            Vector.Equals(this.toCompare, commaVector),
                            Vector.Equals(this.toCompare, rbracketVector)
                            ),
                        Vector.Equals(this.toCompare, rbraceVector)));
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
                for (int x = 0; x <= segmentMinus; x++) {
                    if (eq[x] > 0) {
                        ushort current = buf[x];
                        if (current == COMMA || IsWhiteSpace(current) || current == RBRACE || current == RBRACKET) {
                            this.CurrentIndex += x;
                            return this.CurrentValue;
                        }
                    }
                }
                this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
            } while (this.Increment());
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int AdvanceToEndOfObject() {
            if (this.CurrentValue != LBRACE) {
                this.AdvanceTo(LBRACE);
            }
            int segmentMinus = this.SegmentZeroBased;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[] {
                new Vector<ushort>(QUOTE),
                new Vector<ushort>(LBRACE),
                new Vector<ushort>(LBRACKET),
                new Vector<ushort>(RBRACE),
                new Vector<ushort>(RBRACKET),
            };
            int searchLen = searchVectors.Length;
            bool WithinQuotes = false;
            int debth = 1; // Already within the first brace
            Span<ushort> buf = this.GetToCompareSpan;
            Continue:
            while (this.Increment()) {
                if (this.CurrentIndex + this.Segment > this.endIndex) {
                    buf.Clear(); // Remove trailing chars
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, MemoryMarshal.GetReference(searchVectors));
                if (WithinQuotes) {
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    for (int i = 0; i <= segmentMinus; i++) {
                        if (eq[i] > 0) {
                            WithinQuotes = false;
                            this.CurrentIndex += i; // One Place after Quote
                            goto Continue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                }
                else {
                    for (int i = 1; i < searchLen; i++) {
                        eq = Vector.BitwiseOr(eq, Vector.Equals(this.toCompare, searchVectors[i]));
                    }
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    for (int i = 0; i <= segmentMinus; i++) {
                        if (eq[i] > 0) {
                            ushort ch = buf[i];
                            switch (ch) {
                                case QUOTE: {
                                    WithinQuotes = true;
                                    this.CurrentIndex += i; // One Place after Quote
                                    goto Continue;
                                }
                                case LBRACE:
                                case LBRACKET: {
                                    debth++;
                                    break;
                                }
                                case RBRACKET: {
                                    debth--;
                                    break;
                                }
                                case RBRACE: {
                                    if (--debth == 0) {
                                        this.CurrentIndex += i;
                                        return this.CurrentValue;
                                    }
                                    break;
                                }
                            }
                        }

                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                }
            }
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int AdvanceToEndOfArray() {
            if (this.CurrentValue != LBRACKET) {
                this.AdvanceTo(LBRACKET);
            }
            int segmentMinus = this.Segment - 1;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[] {
                new Vector<ushort>(QUOTE),
                new Vector<ushort>(LBRACE),
                new Vector<ushort>(LBRACKET),
                new Vector<ushort>(RBRACE),
                new Vector<ushort>(RBRACKET),
            };
            int searchLen = searchVectors.Length;
            bool WithinQuotes = false;
            int debth = 1; // Already within the first brace
            Span<ushort> buf = this.GetToCompareSpan;
            Continue:
            while (this.Increment()) {
                if (this.CurrentIndex + this.Segment > this.endIndex) {
                    buf.Clear(); // Remove trailing chars
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                    segmentMinus = this.endIndex - this.CurrentIndex;
                }
                else {
                    this.source.Slice(this.CurrentIndex, this.Segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, MemoryMarshal.GetReference(searchVectors));
                if (WithinQuotes) {
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    for (int i = 0; i <= segmentMinus; i++) {
                        if (eq[i] > 0) {
                            WithinQuotes = false;
                            this.CurrentIndex += i; // One Place after Quote
                            goto Continue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                }
                else {
                    for (int i = 1; i < searchLen; i++) {
                        eq = Vector.BitwiseOr(eq, Vector.Equals(this.toCompare, searchVectors[i]));
                    }
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    for (int i = 0; i <= segmentMinus; i++) {
                        if (eq[i] > 0) {
                            ushort ch = buf[i];
                            switch (ch) {
                                case QUOTE: {
                                    WithinQuotes = true;
                                    this.CurrentIndex += i; // One Place after Quote
                                    goto Continue;
                                }
                                case LBRACE:
                                case LBRACKET: {
                                    debth++;
                                    break;
                                }
                                case RBRACE: {
                                    debth--;
                                    break;
                                }
                                case RBRACKET: {
                                    if (--debth == 0) {
                                        this.CurrentIndex += i;
                                        return this.CurrentValue;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                }
            }
            return '\0';
        }

        #endregion


        #region # RETREAT #

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort Retreat(int amount = 1) {
            while (this.Decrement() && --amount > 0) { }
            return this.CurrentValue;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort Retreat() {
            this.Decrement();
            return this.CurrentValue;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int RetreatTo(ushort search) {
            int segmentMinus = this.Segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(search);
            Span<ushort> buf = this.GetToCompareSpan;
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start >= 0) {
                    this.source.Slice(start, this.Segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                    segmentMinus = this.CurrentIndex;
                    start = 0;
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
                for (int i = segmentMinus; i >= 0; i--) {
                    if (eq[i] > 0) {
                        this.CurrentIndex = start + i;
                        return this.CurrentIndex;
                    }
                }
                this.CurrentIndex -= segmentMinus;
            }
            return -1;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int RetreatToNot(ushort search) {
            int segmentMinus = this.Segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(search);
            Span<ushort> buf = this.GetToCompareSpan;
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start >= 0) {
                    this.source.Slice(start, this.Segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                    segmentMinus = this.CurrentIndex;
                    start = 0;
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentIndex;
                }
                for (int i = this.Segment - 1; i >= 0; i--) {
                    if (eq[i] == 0) {
                        this.CurrentIndex = start + i;
                        return this.CurrentIndex;
                    }
                }
                this.CurrentIndex -= segmentMinus; // Segments is one base so dont increment
            }
            return -1;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort RetreatTo(in ReadOnlySpan<ushort> search) {
            int segmentMinus = this.Segment - 1;
            int searchLen = search.Length;
            Span<ushort> buf = this.GetToCompareSpan;
            Span<Vector<ushort>> searchVector = stackalloc Vector<ushort>[searchLen];
            for (int i = 0; i < searchLen; i++) {
                searchVector[i] = new Vector<ushort>(search[i]);
            }
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start >= 0) {
                    this.source.Slice(start, this.Segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                    segmentMinus = this.CurrentIndex;
                    start = 0;
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, MemoryMarshal.GetReference(searchVector));
                for (int i = 1; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(this.toCompare, searchVector[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
                for (int i = this.Segment - 1; i >= 0; i--) {
                    if (eq[i] > 0) {
                        this.CurrentIndex = start + i;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex -= segmentMinus;
            }
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort RetreatToNot(in ReadOnlySpan<ushort> search) {
            int segmentMinus = this.Segment - 1;
            int searchLen = search.Length;
            Span<ushort> buf = this.GetToCompareSpan;
            Span<Vector<ushort>> searchVector = stackalloc Vector<ushort>[searchLen];
            for (int i = 0; i < searchLen; i++) {
                searchVector[i] = new Vector<ushort>(search[i]);
            }
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start >= 0) {
                    this.source.Slice(start, this.Segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                    segmentMinus = this.CurrentIndex;
                    start = 0;
                }
                Vector<ushort> eq = Vector.Equals(this.toCompare, MemoryMarshal.GetReference(searchVector));
                for (int i = 1; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(this.toCompare, searchVector[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                for (int i = this.Segment - 1; i >= 0; i--) {
                    if (eq[i] == 0) {
                        this.CurrentIndex = start + i;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex -= segmentMinus;
            }
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort RetreatToWhiteSpace() {
            int segmentMinus = this.Segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(32); // 32 == ' ' 
            Span<ushort> buf = this.GetToCompareSpan;
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start >= 0) {
                    this.source.Slice(start, this.Segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                    segmentMinus = this.CurrentIndex;
                    start = 0;
                }
                Vector<ushort> eq = Vector.GreaterThan(this.toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                for (int i = this.Segment - 1; i >= 0; i--) {
                    if (eq[i] == 0 && IsWhiteSpace(buf[i])) {
                        this.CurrentIndex = start + i;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex -= segmentMinus;
            }
            return '\0';
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort RetreatToNotWhiteSpace() {
            int segmentMinus = this.Segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(33); // 32 == ' '
            Span<ushort> buf = this.GetToCompareSpan;
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start >= 0) {
                    this.source.Slice(start, this.Segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                    segmentMinus = this.CurrentIndex;
                    start = 0;
                }
                Vector<ushort> eq = Vector.LessThan(this.toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                for (int i = this.Segment - 1; i >= 0; i--) {
                    if (eq[i] == 0 && !IsWhiteSpace(buf[i])) {
                        this.CurrentIndex = start + i;
                        return this.CurrentValue;
                    }
                }
                this.CurrentIndex -= segmentMinus;
            }
            return '\0';
        }

        #endregion

    }

    #endregion

    #region ### NanoJsonStatics ###

    public static class NanoJsonStatics {

        public const string INDENT_TABS = "   ";
        public const int INDENT_LEN = 3;
        public const string NULL = "null";
        public static string TRUE_u => bool.TrueString;
        public const string TRUE_l = "true";
        public static string FALSE_u => bool.FalseString;
        public const string FALSE_l = "false";

        internal const ushort QUOTE = '"';
        internal const ushort LBRACKET = '[';
        internal const ushort RBRACKET = ']';
        internal const ushort LBRACE = '{';
        internal const ushort RBRACE = '}';
        internal const ushort COLON = ':';
        internal const ushort COMMA = ',';
        internal const ushort T_LOWER = 't';
        internal const ushort T_UPPER = 'T';
        internal const ushort F_LOWER = 'f';
        internal const ushort F_UPPER = 'F';
        internal const ushort N_LOWER = 'n';
        internal const ushort N_UPPER = 'N';
        internal const ushort E_LOWER = 'e';
        internal const ushort E_UPPER = 'E';
        internal const ushort U_LOWER = 'u';
        internal const ushort U_UPPER = 'U';
        internal const ushort L_LOWER = 'l';
        internal const ushort L_UPPER = 'L';
        internal const ushort R_LOWER = 'r';
        internal const ushort R_UPPER = 'R';
        internal const ushort A_LOWER = 'a';
        internal const ushort A_UPPER = 'A';
        internal const ushort S_LOWER = 's';
        internal const ushort S_UPPER = 'S';

        public const ulong JSONWHITESPACEMASK = (1UL << 9) | (1UL << 10) | (1UL << 13) | (1UL << 32);

        /// <summary>
        /// Format used by the basic <c>.ToString()</c>
        /// </summary>
        public static ToStringFormat Default_ToStringFormat = ToStringFormat.Default;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static T GetConvertedValue<T>(double value) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            unchecked {
                switch (Type.GetTypeCode(typeof(T))) {
                    case TypeCode.SByte: { return (sbyte)value is T r ? r : default; }
                    case TypeCode.Byte: { return (byte)value is T r ? r : default; }
                    case TypeCode.Int16: { return (short)value is T r ? r : default; }
                    case TypeCode.UInt16: { return (ushort)value is T r ? r : default; }
                    case TypeCode.Int32: { return (int)value is T r ? r : default; }
                    case TypeCode.UInt32: { return (uint)value is T r ? r : default; }
                    case TypeCode.Int64: { return (long)value is T r ? r : default; }
                    case TypeCode.UInt64: { return (ulong)value is T r ? r : default; }
                    case TypeCode.Single: { return (float)value is T r ? r : default; }
                    case TypeCode.Double: { return value is T r ? r : default; }
                    case TypeCode.Decimal: { return (decimal)value is T r ? r : default; }
                    default:
                        throw new NotSupportedException(typeof(T).Name);
                }
            }
        }

        public static string GetStringDecodedFromSpan(in ReadOnlySpan<char> data) {
            int len = data.Length;
            int x = 0;
            if (len <= 256) { // fast-path for small strings: stackalloc to avoid ArrayPool rent
                Span<char> stackBuffer = stackalloc char[len];
                TranslateUnicodeIntoBufferFromSpan(in data, in stackBuffer, ref x);
                return new string(stackBuffer.Slice(0, x));
            }
            char[] buffer = ArrayPool<char>.Shared.Rent(len);
            Span<char> bufSpan = buffer.AsSpan(0, len);
            TranslateUnicodeIntoBufferFromSpan(in data, in bufSpan, ref x);
            string decodedValue = new string(bufSpan.Slice(0, x));
            ArrayPool<char>.Shared.Return(buffer);
            return decodedValue;
        }

        public static int GetStringDecodeLengthFromSpan(in ReadOnlySpan<char> data) {
            JsonReader reader = new JsonReader(data, true);
            int len = data.Length;
            if (reader.RetreatTo('\\') < 0) {
                return len;
            }
            int count = len;
            do {
                int nextIndex = reader.CurrentIndex + 1;
                if (nextIndex < len) {
                    switch (data[nextIndex]) {
                        case 'u':
                            if (nextIndex + 4 < len) {
                                count -= 5; // -4 Hex numbers, +1 from 'u', '\' not counted
                            }
                            break;
                        case 'n':
                        case 't':
                        case 'r':
                        case 'f':
                        case 'b':
                        case 'a':
                            count -= 1; // -1 for the escape character
                            break;
                    }
                }
            } while (reader.RetreatTo('\\') >= 0);
            return count;
        }

        public static void TranslateUnicodeIntoBufferFromSpan(in ReadOnlySpan<char> data, in Span<char> buffer, ref int sbPos) {
            JsonReader reader = new JsonReader(data);
            int len = data.Length;
            if (reader.AdvanceTo('\\') < 0) {
                data.CopyTo(buffer.Slice(sbPos, len));
                sbPos += len;
                return;
            }

            int currentStart = 0;
            int endIndex = len - 1;
            do {
                int currentIndex = reader.CurrentIndex;
                int currentLen = currentIndex - currentStart;
                if (currentLen > 0) {
                    data.Slice(currentStart, currentLen).CopyTo(buffer.Slice(sbPos, currentLen));
                    sbPos += currentLen;
                }
                if (currentIndex >= endIndex) {
                    break;
                }
                else {
                    char current = data[++currentIndex];
                    switch (current) {
                        case 'n':
                            buffer[sbPos++] = '\n';
                            break;
                        case 't':
                            buffer[sbPos++] = '\t';
                            break;
                        case 'u': {
                            if (currentIndex + 4 < len) {
                                buffer[sbPos++] = (char)((ReadHexNumber(data[++currentIndex]) * 4096)
                                    + (ReadHexNumber(data[++currentIndex]) * 256)
                                    + (ReadHexNumber(data[++currentIndex]) * 16)
                                    + ReadHexNumber(data[++currentIndex]));
                            }
                            else {
                                buffer[sbPos++] = current;
                            }
                            break;
                        }
                        case 'r':
                            buffer[sbPos++] = '\r';
                            break;
                        case 'f':
                            buffer[sbPos++] = '\f';
                            break;
                        case 'b':
                            buffer[sbPos++] = '\b';
                            break;
                        case 'a':
                            buffer[sbPos++] = '\a';
                            break;
                        default:
                            buffer[sbPos++] = current;
                            break;
                    }
                }
                currentStart = ++currentIndex;
                reader.SetIndexPosition(currentIndex);
            } while (reader.AdvanceTo('\\') > 0);

            int remainingLen = len - currentStart;
            if (remainingLen > 0) {
                data.Slice(currentStart, remainingLen).CopyTo(buffer.Slice(sbPos, remainingLen));
                sbPos += remainingLen;
            }
        }

        /// <summary>
        /// <c>USES ARRAYPOOL ARRAYS</c>, do not insert normal arrays
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="nextIndex"></param>
        /// <param name="buffer"></param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void EnsureBufferCapacity(int nextIndex, ref char[] buffer) {
            if (nextIndex >= buffer.Length) {
                char[] newArray = ArrayPool<char>.Shared.Rent(nextIndex + 1);
                buffer.CopyTo(newArray.AsSpan(0, buffer.Length));
                ArrayPool<char>.Shared.Return(buffer);
                buffer = newArray;
            }
        }

        /// <summary>
        /// Returns true if char value is less than 33
        /// </summary>
        /// <param name="character"></param>
        /// <returns></returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsWhiteSpace(char character) {
            return IsWhiteSpace((ushort)character);
        }

        /// <summary>
        /// Returns true if char value complies with <c>RFC 8259</c> for a whitespace character
        /// </summary>
        /// <param name="character"></param>
        /// <returns></returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsWhiteSpace(ushort character) {
            return (JSONWHITESPACEMASK & (1UL << character)) != 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int ReadHexNumber(char character) {
            switch (character) {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    return character - 48;
                case 'A':
                case 'B':
                case 'C':
                case 'D':
                case 'E':
                case 'F':
                    return character - 55;
                case 'a':
                case 'b':
                case 'c':
                case 'd':
                case 'e':
                case 'f':
                    return character - 87;
                default:
                    throw new FormatException(nameof(character));
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int ComputeHash(in ReadOnlySpan<char> data, out int len) {
            int hash = 17; // Initial hash value
            len = data.Length;
            for (int x = len; --x >= 0;) {
                hash = hash * 31 + data[x]; // Combine hash with character
            }
            return hash; // Return the final hash value
        }

        /// <summary>
        /// Checks if left has flag value of right
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool HasFlag(long left, long right) {
            return (left & right) > 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static double ParseNumber(in ReadOnlySpan<char> data) {
            double returnNumber = double.NaN;
            int len = data.Length;
            if (len > 0) {
                int pos = 0;

                // handle sign
                char c = data[pos];
                bool negative = c == '-';
                if (negative || c == '+') {
                    pos++;
                }

                // find 'e' or 'E' for exponent (if any)
                int eIndex = -1;
                for (int x = pos; x < len; x++) {
                    c = data[x];
                    if (c == 'e' || c == 'E') {
                        eIndex = x;
                        break;
                    }
                }

                // main part (integer + fractional) excludes exponent
                ReadOnlySpan<char> mainSpan = eIndex > 0 ? data.Slice(pos, eIndex - pos) : data.Slice(pos);

                // parse integer and fractional parts
                double value = 0.0d;
                bool seenDecimal = false;
                double fracDiv = 1.0;
                for (int x = 0; x < mainSpan.Length; x++) {
                    c = mainSpan[x];
                    if (c == '.') {
                        seenDecimal = true;
                        continue;
                    }
                    int digit = ReadHexNumber(c); // expects '0' to '9'
                    if (!seenDecimal) {
                        value = value * 10.0 + digit;
                    }
                    else {
                        fracDiv *= 10.0;
                        value += digit / fracDiv;
                    }
                }

                // parse exponent (if present) and apply
                if (eIndex > 0) {
                    int expPos = eIndex + 1;
                    bool expNegative = false;
                    if (expPos < len) {
                        char ec = data[expPos];
                        expNegative = ec == '-';
                        if (expNegative || ec == '+') {
                            expPos++;
                        }
                    }
                    int expVal = 0;
                    for (int x = expPos; x < len; x++) {
                        expVal = expVal * 10 + ReadHexNumber(data[x]);
                    }
                    if (expNegative) {
                        expVal = -expVal;
                    }
                    value *= Math.Pow(10.0, expVal);
                }

                returnNumber = negative ? -value : value;
            }
            return returnNumber;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsNumber(ReadOnlySpan<char> data) {
            int len = data.Length;
            if (len == 0) {
                return false;
            }
            int index = -1;
            bool dec = false;
            bool EFound = false;
            ref char firstChar = ref MemoryMarshal.GetReference(data);
            if (firstChar == '-' || firstChar == '+') {
                index++;
            }

            while (++index < len) {
                switch (data[index]) {
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9': {
                        continue;
                    }
                    case '.': {
                        if (dec) {
                            return false;
                        }
                        dec = true;
                        continue;
                    }
                    case 'e':
                    case 'E': {
                        if (EFound) {
                            return false;
                        }
                        else {
                            char c = data[++index];
                            if ((c ^ '+') == 0 || (c ^ '-') == 0) {
                                EFound = true;
                                continue;
                            }
                            else {
                                return false;
                            }
                        }
                    }
                    default:
                        return false;
                }
            }
            return true;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void EnsureBufferCapacity<T>(int nextIndex, ref T[] buffer, ArrayPool<T> existingPool = null) {
            if (nextIndex >= buffer.Length) {
                existingPool ??= ArrayPool<T>.Shared;
                T[] newArray = existingPool.Rent(nextIndex + 1);
                buffer.CopyTo(newArray.AsSpan(0, buffer.Length));
                existingPool.Return(buffer, true); // Release memory references
                buffer = newArray;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int RoundUpToPowerOf2(int value) {
            uint uvalue = Convert.ToUInt32(value);
            if (uvalue <= 1) {
                return 1;
            }
            uint upper = 1;
            while (upper < uvalue) {
                upper <<= 1;
            }

            return Convert.ToInt32(upper);
        }

        public static class JsonContainerPool {
            private const int knownMax = 1 << 20; // ArrayPool Shared max, adjust higher or lower to needs
            //private const int knownMin = 1 << 4; // ArrayPool Shared min
            private const int knownMin = 1 << 0;
            private const int defaultArraysPerBucket = ushort.MaxValue;

            private static int poolMaxArrayLength = knownMax;
            public static int PoolMaxArrayLength
            {
                get => poolMaxArrayLength;
                set
                {
                    if (ContainerPool.IsValueCreated) {
                        throw new InvalidOperationException("The ArrayPool has already been created, therefore this value can no longer be updated.");
                    }
                    if (value < knownMin) {
                        poolMaxArrayLength = knownMin;
                    }
                    else if (value > knownMax) {
                        poolMaxArrayLength = knownMax;
                    }
                    else {
                        poolMaxArrayLength = value;
                    }
                }
            }

            private static int poolMinArrayLength = knownMin;
            public static int PoolMinArrayLength
            {
                get => poolMinArrayLength;
                set
                {
                    if (ContainerPool.IsValueCreated) {
                        throw new InvalidOperationException("The ArrayPool has already been created, therefore this value can no longer be updated.");
                    }
                    if (value < knownMin) {
                        poolMinArrayLength = knownMin;
                    }
                    else if (value > knownMax) {
                        poolMinArrayLength = knownMax;
                    }
                    else {
                        poolMinArrayLength = value;
                    }
                }
            }

            private static int arraysPerBucket = defaultArraysPerBucket;
            public static int ArraysPerBucket
            {
                get => arraysPerBucket;
                set
                {
                    if (ContainerPool.IsValueCreated) {
                        throw new InvalidOperationException("The ArrayPool has already been created, therefore this value can no longer be updated.");
                    }
                    if (value < 1) {
                        arraysPerBucket = 1;
                    }
                    else {
                        arraysPerBucket = value;
                    }
                }
            }

            private static readonly Lazy<JsonArrayPool> ContainerPool = new Lazy<JsonArrayPool>(() => new JsonArrayPool(knownMin, PoolMaxArrayLength, ArraysPerBucket));

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal static JsonMemory[] Rent(int length) {
                return ContainerPool.Value.Rent(length);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal static void Return(JsonMemory[] array) {
                ContainerPool.Value.Return(array, true);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal static void EnsureBufferCapacity(int nextIndex, ref JsonMemory[] buffer) {
                if (nextIndex >= buffer.Length) {
                    JsonMemory[] newArray = Rent(nextIndex + 1);
                    buffer.CopyTo(newArray.AsSpan(0, buffer.Length));
                    Return(buffer);
                    buffer = newArray;
                }
            }

            private class JsonArrayPool : ArrayPool<JsonMemory> {

                private readonly ConcurrentDictionary<int, ConcurrentQueue<JsonMemory[]>> Buckets;
                private readonly int lowerBound;
                private readonly int upperBound;
                private readonly int arraysPerBucket;

                public JsonArrayPool(int lowerBound, int upperBound, int arraysPerBucket) {
                    if (lowerBound < 1) {
                        throw new ArgumentOutOfRangeException(nameof(lowerBound), "Lower bound must be greater than 0");
                    }
                    if (upperBound < lowerBound) {
                        throw new ArgumentOutOfRangeException(nameof(upperBound), "Upper bound must be greater than or equal to lower bound");
                    }
                    if (arraysPerBucket < 1) {
                        throw new ArgumentOutOfRangeException(nameof(arraysPerBucket), "Arrays per bucket must be greater than 0");
                    }
                    this.lowerBound = lowerBound;
                    this.upperBound = upperBound;
                    this.arraysPerBucket = arraysPerBucket;

                    int NearestLower = RoundUpToPowerOf2(lowerBound);
                    int NearestUpper = RoundUpToPowerOf2(upperBound);

                    int count = 1;
                    while (NearestLower < NearestUpper) {
                        count++;
                        NearestLower <<= 1;
                    }

                    this.Buckets = new ConcurrentDictionary<int, ConcurrentQueue<JsonMemory[]>>(Environment.ProcessorCount, count);
                    do {
                        this.Buckets.TryAdd(NearestLower, new ConcurrentQueue<JsonMemory[]>());
                        if (--count > 0) {
                            NearestLower >>= 1;
                        }
                    } while (count > 0);
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                public override JsonMemory[] Rent(int minimumLength) {
                    int bucketSize;
                    if (minimumLength < this.lowerBound) {
                        bucketSize = this.lowerBound;
                    }
                    else if (minimumLength > this.upperBound) {
                        return new JsonMemory[RoundUpToPowerOf2(minimumLength)];
                    }
                    else {
                        bucketSize = RoundUpToPowerOf2(minimumLength);
                    }

                    if (this.Buckets.TryGetValue(bucketSize, out ConcurrentQueue<JsonMemory[]> queue)) {
                        if (queue.TryDequeue(out JsonMemory[] array)) {
                            return array;
                        }
                        else {
                            return new JsonMemory[bucketSize];
                        }
                    }
                    else {
                        throw new InvalidOperationException("No bucket found for the specified size.");
                    }
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                public void Return(JsonMemory[] array) {
                    this.Return(array, true);
                }

                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                public override void Return(JsonMemory[] array, bool clearArray = false) {
                    int len = array.Length;
                    if (len < this.lowerBound || len > this.upperBound || len != RoundUpToPowerOf2(len)) {
                        return; // Ignore arrays outside the managed range
                    }

                    if (clearArray) {
                        Array.Clear(array, 0, len);
                    }
                    if (this.Buckets.TryGetValue(len, out ConcurrentQueue<JsonMemory[]> queue)) {
                        queue.Enqueue(array);
                        while (queue.Count > this.arraysPerBucket) {
                            queue.TryDequeue(out _);
                        }
                    }
                    else {
                        throw new InvalidOperationException("No bucket found for the specified size.");
                    }
                }
            }
        }
    }

    #endregion

    #region ### ToStringFormat ###

    [Serializable, Flags]
    public enum ToStringFormat : long {
        None = 0,
        Pretty = 0x1,
        /// <summary>
        /// Translates Unicode inside strings to their actual character representation
        /// </summary>
        TranslateUnicode = 0x2,
        /// <summary>
        /// The Default C# Bool is <c>True</c> and <c>False</c>, this option changes the output to <c>true</c> and <c>false</c> to comply with JSON standards, this option is ignored when parsing as both are accepted
        /// </summary>
        LowerCaseBool = 0x4,
        /// <summary>
        /// Numbers already pass the numerical test for validity, this option gets the number representation from <c>double</c> instead of the reference
        /// </summary>
        ReParseNumbers = 0x8,
        /// <summary>
        /// Null values will be represented as empty references instead of the string "null", this is not compliant with JSON standards but can be useful for some applications, objects and arrays will still represent null values within them as the string "null"
        /// </summary>
        NullReturnsEmptyReference = 0x10,

        /// <summary>
        /// Starting value of <c>Default_ToStringFormat</c>
        /// </summary>
        Default = Pretty | TranslateUnicode,
        All = Pretty | TranslateUnicode | LowerCaseBool | ReParseNumbers,
    }

    #endregion

    #region ### JsonType ###

    [Serializable, Flags]
    public enum JsonType : long {
        /// <summary>
        /// <c>null</c>
        /// </summary>
        Null = 0x1,
        /// <summary>
        /// <c>NanoJson[]</c>
        /// </summary>
        Object = 0x2,
        /// <summary>
        /// <c>NanoJson[]</c>
        /// </summary>
        Array = 0x4,
        /// <summary>
        /// <c>string</c>
        /// </summary>
        String = 0x8,
        /// <summary>
        /// <c>bool</c>
        /// </summary>
        Boolean = 0x10,
        /// <summary>
        /// <c>double</c>
        /// </summary>
        Number = 0x20,
        /// <summary>
        /// Internal flag to determin if the internal array was rented and can be returned
        /// </summary>
        Disposable = 0x40,

        Container = Object | Array,
        Value = String | Boolean | Number | Null,
        DateTime = String | Number,
    }

    #endregion

    #region ### MemoryJsonExtensions ###

    public static class NanoJsonExtensions {
        public static JsonMemory ToJsonObject(this JsonMemory[] objects, int innerLength = -1, string key = "") {
            if (string.IsNullOrWhiteSpace(key)) {
                return JsonMemory.CreateObject(objects);
            }
            else {
                return JsonMemory.CreateObject(key, objects);
            }
        }

        public static JsonMemory ToJsonArray(this JsonMemory[] array, int innerLength = -1, string key = "") {
            if (string.IsNullOrWhiteSpace(key)) {
                return JsonMemory.CreateArray(array);
            }
            else {
                return JsonMemory.CreateArray(key, array);
            }
        }

        public static JsonMemory ToJsonArray(this string[] strings, int innerLength = -1, string key = "") {
            int len = strings.Length;
            JsonMemory[] array = new JsonMemory[len];
            for (int x = 0; x < len; x++) {
                array[x] = JsonMemory.CreateString(strings[x]);
            }
            if (string.IsNullOrWhiteSpace(key)) {
                return JsonMemory.CreateArray(array);
            }
            else {
                return JsonMemory.CreateArray(key, array);
            }
        }

        public static JsonMemory AsJsonMemory(this string str) {
            return JsonMemory.CreateString(str);
        }

        public static JsonSpan AsJsonSpan(this string str) {
            return new JsonSpan(str);
        }

        public static JsonMemory AsJsonMemory(this double number) {
            return JsonMemory.CreateNumber(number);
        }

        public static JsonMemory AsJsonMemory(this bool b) {
            return JsonMemory.CreateBool(b);
        }

        public static JsonMemory ParseJson(this string json) {
            return JsonMemory.ParseJson(json);
        }
    }
    #endregion
}
