///////////////////////////////////////////////////////////////
///															///
///		NanoJson by Duncan 'ScoredOne' Mellor				///
///															///
///		Released under the MIT license						///
///															///
///		Thursday 27th November 2025							///
///															///
///		Software provided in as-is condition				///
///															///
///		Git: https://github.com/ScoredOne/NanoJson			///
///															///
///		Nuget: https://www.nuget.org/packages/NanoJson		///
///															///
///////////////////////////////////////////////////////////////

using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading;

using static NanoJson.NanoJsonStatics;

namespace NanoJson {

    #region ### JsonSpan ###

    [StructLayout(LayoutKind.Sequential)]
    public ref struct JsonSpan {

        [StructLayout(LayoutKind.Sequential)]
        public ref struct Enumerator {
            private readonly ReadOnlySpan<char> ownerValue;
            private JsonSpan current;
            private JsonReader reader;
            private readonly JsonType ownerType;
            private int index;

            internal Enumerator(in JsonSpan owner) {
                this.ownerValue = owner.Value;
                this.ownerType = owner.Type;
                JsonReader r = new JsonReader(owner.Value);
                this.index = -1;
                this.current = Empty;
                switch (this.ownerType) {
                    case JsonType.Object:
                        r.AdvanceTo(LBRACE);
                        break;
                    case JsonType.Array:
                        r.AdvanceTo(LBRACKET);
                        break;
                }
                this.reader = r;
            }

            public readonly JsonSpan Current => this.current;

            public JsonSpan this[in int index]
            {
                get
                {
                    if (this.TryGetIndex(in index, out JsonSpan value)) {
                        return value;
                    }
                    throw new IndexOutOfRangeException();
                }
            }

            public bool TryGetIndex(in int index, out JsonSpan value) {
                if (index < 0) {
                    throw new ArgumentOutOfRangeException(nameof(index));
                }
                if (index == this.index) {
                    value = this.current;
                    return true;
                }
                int endLen = this.index;
                bool found = false;
                if (this.index > 0) {
                    while (this.MoveNext()) {
                        if (this.index == index) {
                            found = true;
                            goto Found;
                        }
                    }
                    this.Reset();
                    for (int x = 0; x < endLen; x++) {
                        this.MoveNext();
                        if (this.index == index) {
                            found = true;
                            goto Found;
                        }
                    }
                }
                else {
                    while (this.MoveNext()) {
                        if (this.index == index) {
                            found = true;
                            goto Found;
                        }
                    }
                }
                Found:
                if (found) {
                    value = this.current;
                    return true;
                }
                else {
                    value = Empty;
                    return false;
                }
            }

            public JsonSpan this[in ReadOnlySpan<char> key]
            {
                get
                {
                    if (this.TryGetKey(in key, out JsonSpan value)) {
                        return value;
                    }
                    throw new ArgumentException("Key not found", nameof(key));
                }
            }

            public bool TryGetKey(in ReadOnlySpan<char> key, out JsonSpan value) {
                if (key.IsEmpty) {
                    throw new ArgumentOutOfRangeException(nameof(key));
                }
                if (this.ownerType != JsonType.Object) {
                    value = Empty;
                    return false;
                }
                if (this.index >= 0 && key.StartsWith(this.current.Key, StringComparison.Ordinal)) {
                    value = this.current;
                    return true;
                }
                int endLen = this.index;
                bool found = false;
                if (this.index > 0) {
                    while (this.MoveNext()) {
                        if (key.StartsWith(this.current.Key, StringComparison.Ordinal)) {
                            found = true;
                            goto Found;
                        }
                    }
                    this.Reset();
                    for (int x = 0; x < endLen; x++) {
                        this.MoveNext();
                        if (key.StartsWith(this.current.Key, StringComparison.Ordinal)) {
                            found = true;
                            goto Found;
                        }
                    }
                }
                else {
                    while (this.MoveNext()) {
                        if (key.StartsWith(this.current.Key, StringComparison.Ordinal)) {
                            found = true;
                            goto Found;
                        }
                    }
                }
                Found:
                if (found) {
                    if (key.Length == this.current.GetKeyLen) {
                        value = this.current;
                    }
                    else {
                        this.current.TryGetKey(key.Slice(this.current.GetKeyLen + 1), out value);
                    }
                    return true;
                }
                else {
                    value = Empty;
                    return false;
                }
            }

            public bool MoveNext() {
                switch (this.ownerType) {
                    case JsonType.Object: {
                        ref JsonReader providedReader = ref this.reader;
                        providedReader.AdvanceToNotWhiteSpace();
                        if (providedReader.CurrentValue != QUOTE) {
                            providedReader.AdvanceTo(QUOTE);
                        }
                        ReadOnlySpan<char> name;
                        int left;
                        if (providedReader.Advance() == QUOTE) {
                            name = ReadOnlySpan<char>.Empty;
                        }
                        else {
                            left = providedReader.CurrentIndex;
                            providedReader.AdvanceTo(QUOTE);
                            name = this.ownerValue.Slice(left, providedReader.CurrentIndex - left);
                        }
                        providedReader.AdvanceTo(COLON);
                        providedReader.AdvanceToNotWhiteSpace();
                        left = providedReader.CurrentIndex;
                        this.index++;
                        this.current = new JsonSpan(name, ref providedReader);
                        switch (providedReader.CurrentValue) {
                            case COMMA:
                            case RBRACE:
                            case RBRACKET:
                                break;
                            default: {
                                providedReader.AdvanceToCommaOrEndBrace();
                                break;
                            }
                        }
                        if (left == providedReader.CurrentIndex) {
                            return false;
                        }
                        return true;
                    }
                    case JsonType.Array: {
                        ref JsonReader providedReader = ref this.reader;
                        providedReader.AdvanceToNotWhiteSpace();
                        int left = providedReader.CurrentIndex;
                        this.index++;
                        this.current = new JsonSpan(ReadOnlySpan<char>.Empty, ref providedReader);
                        switch (providedReader.CurrentValue) {
                            case COMMA:
                            case RBRACE:
                            case RBRACKET:
                                break;
                            default: {
                                providedReader.AdvanceToCommaOrEndBrace();
                                break;
                            }
                        }
                        if (left == providedReader.CurrentIndex) {
                            return false;
                        }
                        return true;
                    }
                    default:
                        return false;
                }
            }

            public void Reset() {
                this.index = -1;
                this.reader.SetIndexToStart();
            }
        }

        public static JsonSpan Empty => new JsonSpan(true);

        public readonly ReadOnlySpan<char> Key;
        public readonly ReadOnlySpan<char> Value;
        public readonly JsonType Type;
        public readonly bool IsEmpty;

        public readonly bool IsNothing => this.Key == ReadOnlySpan<char>.Empty && this.Value == ReadOnlySpan<char>.Empty;

        private JsonSpan(bool _) {
            this.Type = JsonType.Null;
            this.Key = ReadOnlySpan<char>.Empty;
            this.Value = ReadOnlySpan<char>.Empty;
            this.IsEmpty = true;
        }

        public JsonSpan(string data) : this(data.AsSpan()) { }
        public JsonSpan(in ReadOnlySpan<char> read) : this(ReadOnlySpan<char>.Empty, read) { }

        public JsonSpan(string key, string value) : this(key.AsSpan(), value.AsSpan()) { }
        public JsonSpan(in ReadOnlySpan<char> key, in ReadOnlySpan<char> read) {
            if (read.IsEmpty) {
                this = Empty;
                this.Key = key;
                return;
            }
            this.Key = key;
            int len = read.Length;
            ReadOnlySpan<ushort> data = MemoryMarshal.Cast<char, ushort>(read);
            int x = -1;
            while (IsWhiteSpace(data[++x])) { }
            while (IsWhiteSpace(data[--len])) { }
            len++;
            switch (data[x]) {
                case QUOTE: {
                    this.Type = JsonType.String;
                    while (true) {
                        if (data[--len] == QUOTE) {
                            break;
                        }
                    }
                    if (len == ++x) {
                        this.Value = ReadOnlySpan<char>.Empty;
                        this.IsEmpty = true;
                    }
                    else {
                        this.Value = read.Slice(x, len - x);
                        this.IsEmpty = false;
                    }
                    return;
                }
                case LBRACKET: {
                    this.Type = JsonType.Array;
                    int f = x;
                    while (true) {
                        if (data[--len] == RBRACKET) {
                            break;
                        }
                    }
                    while (IsWhiteSpace(read[++x])) { }
                    if (x == len) {
                        this.Value = ReadOnlySpan<char>.Empty;
                        this.IsEmpty = true;
                    }
                    else {
                        this.Value = read.Slice(f, len - f + 1);
                        this.IsEmpty = false;
                    }
                    return;
                }
                case LBRACE: {
                    this.Type = JsonType.Object;
                    int f = x;
                    while (true) {
                        if (data[--len] == RBRACE) {
                            break;
                        }
                    }
                    while (IsWhiteSpace(data[++x])) { }
                    if (x == len) {
                        this.Value = ReadOnlySpan<char>.Empty;
                        this.IsEmpty = true;
                    }
                    else {
                        this.Value = read.Slice(f, len - f + 1);
                        this.IsEmpty = false;
                    }
                    return;
                }
                case N_LOWER:
                case N_UPPER: {
                    this.Type = JsonType.Null;
                    this.Value = NULL.AsSpan();
                    this.IsEmpty = false;
                    if (len - x == 4 && this.Value.Equals(read.Slice(x, 4), StringComparison.OrdinalIgnoreCase)) {
                        return;
                    }

                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {read.ToString()})", nameof(read));
                }
                case T_LOWER:
                case T_UPPER: {
                    this.Type = JsonType.Boolean;
                    this.Value = bool.TrueString.AsSpan();
                    this.IsEmpty = false;
                    if (len - x == 4 && this.Value.Equals(read.Slice(x, 4), StringComparison.OrdinalIgnoreCase)) {
                        return;
                    }

                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {read.ToString()})", nameof(read));
                }
                case F_LOWER:
                case F_UPPER: {
                    this.Type = JsonType.Boolean;
                    this.Value = bool.FalseString.AsSpan();
                    this.IsEmpty = false;
                    if (len - x == 5 && this.Value.Equals(read.Slice(x, 5), StringComparison.OrdinalIgnoreCase)) {
                        return;
                    }

                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {read.ToString()})", nameof(read));
                }
                default: {
                    this.Type = JsonType.Number;
                    this.IsEmpty = false;
                    this.Value = read.Slice(x, len - x);

                    if (IsNumber(this.Value)) {
                        return;
                    }
                    throw new ArgumentException($"Parse failed (TryParse: {read.ToString()})", nameof(read));
                }
            }
            throw new ArgumentException($"Parse failed (TryParse: {read.ToString()})", nameof(read));
        }

        /// <summary>
        /// Enumerator / JsonReader Specific constructor
        /// </summary>
        /// <param name="key"></param>
        /// <param name="reader"></param>
        /// <exception cref="ArgumentException"></exception>
        private JsonSpan(in ReadOnlySpan<char> key, ref JsonReader reader) {
            if (!reader.CanAdvance) {
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
                    int left = reader.CurrentIndex + 1;
                    reader.AdvanceTo(QUOTE);
                    if (left == reader.CurrentIndex) {
                        this.Value = ReadOnlySpan<char>.Empty;
                        this.IsEmpty = true;
                    }
                    else {
                        this.Value = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, reader.CurrentIndex - left));
                        this.IsEmpty = false;
                    }
                    return;
                }
                case LBRACKET: {
                    this.Type = JsonType.Array;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToEndOfArray();
                    if (reader.CurrentValue != RBRACKET) {
                        throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                    }
                    reader.Increment();
                    ReadOnlySpan<ushort> container = reader.Slice(left, reader.CurrentIndex - left);
                    this.Value = MemoryMarshal.Cast<ushort, char>(container);
                    this.IsEmpty = true;
                    int end = container.Length - 1;
                    for (int x = 1; x < end; x++) {
                        if (!IsWhiteSpace(container[x])) {
                            this.IsEmpty = false;
                            return;
                        }
                    }
                    return;
                }
                case LBRACE: {
                    this.Type = JsonType.Object;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToEndOfObject();
                    if (reader.CurrentValue != RBRACE) {
                        throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                    }
                    reader.Increment();
                    ReadOnlySpan<ushort> container = reader.Slice(left, reader.CurrentIndex - left);
                    this.Value = MemoryMarshal.Cast<ushort, char>(container);
                    this.IsEmpty = true;
                    int end = container.Length - 1;
                    for (int x = 1; x < end; x++) {
                        if (!IsWhiteSpace(container[x])) {
                            this.IsEmpty = false;
                            return;
                        }
                    }
                    return;
                }
                case N_LOWER:
                case N_UPPER: {
                    this.Type = JsonType.Null;
                    this.Value = NULL.AsSpan();
                    this.IsEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    ReadOnlySpan<char> toCompare = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, reader.CurrentIndex - left));
                    if (toCompare.Length == 4 && this.Value.Equals(toCompare, StringComparison.OrdinalIgnoreCase)) {
                        return;
                    }

                    throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                }
                case T_LOWER:
                case T_UPPER: {
                    this.Type = JsonType.Boolean;
                    this.Value = bool.TrueString.AsSpan();
                    this.IsEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    ReadOnlySpan<char> toCompare = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, reader.CurrentIndex - left));
                    if (toCompare.Length == 4 && this.Value.Equals(toCompare, StringComparison.OrdinalIgnoreCase)) {
                        return;
                    }

                    throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                }
                case F_LOWER:
                case F_UPPER: {
                    this.Type = JsonType.Boolean;
                    this.Value = bool.FalseString.AsSpan();
                    this.IsEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    ReadOnlySpan<char> toCompare = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, reader.CurrentIndex - left));
                    if (toCompare.Length == 5 && this.Value.Equals(toCompare, StringComparison.OrdinalIgnoreCase)) {
                        return;
                    }

                    throw new ArgumentException($"Parse failed (TryParse: {reader.ToString()})", nameof(reader));
                }
                default: {
                    this.Type = JsonType.Number;
                    this.IsEmpty = false;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    this.Value = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, reader.CurrentIndex - left));

                    if (IsNumber(this.Value)) {
                        return;
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
                if (this.IsEmpty) {
                    throw new IndexOutOfRangeException("Body is Empty");
                }
                if (this.TryGetIndex(in index, out JsonSpan v)) {
                    return v;
                }
                throw new IndexOutOfRangeException();
            }
        }

        public readonly bool TryGetIndex(in int index, out JsonSpan value) {
            if (!HasFlag((long)JsonType.Container, (long)this.Type)) {
                value = Empty;
                return false;
            }

            JsonReader providedReader = new JsonReader(this.Value);
            int currentIndex = -1;
            switch (this.Type) {
                case JsonType.Object: {
                    providedReader.AdvanceTo(LBRACE);
                    do {
                        if (!providedReader.CanAdvance) {
                            value = Empty;
                            return false;
                        }
                        providedReader.AdvanceToNotWhiteSpace();
                        if (providedReader.CurrentValue != QUOTE) {
                            providedReader.AdvanceTo(QUOTE);
                        }
                        ReadOnlySpan<char> name;
                        if (providedReader.Advance() == QUOTE) {
                            name = ReadOnlySpan<char>.Empty;
                        }
                        else {
                            int left = providedReader.CurrentIndex;
                            providedReader.AdvanceTo(QUOTE);
                            name = this.Value.Slice(left, providedReader.CurrentIndex - left);
                        }
                        providedReader.AdvanceTo(COLON);
                        providedReader.AdvanceToNotWhiteSpace();
                        value = new JsonSpan(name, ref providedReader);
                        switch (providedReader.CurrentValue) {
                            case COMMA:
                            case RBRACE:
                            case RBRACKET:
                                break;
                            default: {
                                providedReader.AdvanceToCommaOrEndBrace();
                                break;
                            }
                        }
                    } while (++currentIndex < index);
                    return true;
                }
                case JsonType.Array: {
                    providedReader.AdvanceTo(LBRACKET);
                    do {
                        if (!providedReader.CanAdvance) {
                            value = Empty;
                            return false;
                        }
                        providedReader.AdvanceToNotWhiteSpace();
                        int left = providedReader.CurrentIndex;
                        value = new JsonSpan(ReadOnlySpan<char>.Empty, ref providedReader);
                        switch (providedReader.CurrentValue) {
                            case COMMA:
                            case RBRACE:
                            case RBRACKET:
                                break;
                            default: {
                                providedReader.AdvanceToCommaOrEndBrace();
                                break;
                            }
                        }
                    } while (++currentIndex < index);
                    return true;
                }
                default:
                    value = Empty;
                    return false;
            }
        }

        public readonly JsonSpan this[in ReadOnlySpan<char> key]
        {
            get
            {
                if (this.IsEmpty) {
                    throw new IndexOutOfRangeException("Body is Empty");
                }
                if (this.TryGetKey(in key, out JsonSpan v)) {
                    return v;
                }
                throw new IndexOutOfRangeException();
            }
        }

        public readonly bool TryGetKey(in ReadOnlySpan<char> key, out JsonSpan value) {
            if (this.Type != JsonType.Object) {
                value = Empty;
                return false;
            }

            JsonReader providedReader = new JsonReader(this.Value);
            providedReader.AdvanceTo(LBRACE);
            ReadOnlySpan<char> name;
            do {
                if (!providedReader.CanAdvance) {
                    value = Empty;
                    return false;
                }
                providedReader.AdvanceToNotWhiteSpace();
                if (providedReader.CurrentValue != QUOTE) {
                    providedReader.AdvanceTo(QUOTE);
                }
                if (providedReader.Advance() == QUOTE) {
                    name = ReadOnlySpan<char>.Empty;
                }
                else {
                    int left = providedReader.CurrentIndex;
                    providedReader.AdvanceTo(QUOTE);
                    name = this.Value.Slice(left, providedReader.CurrentIndex - left);
                }
                providedReader.AdvanceTo(COLON);
                providedReader.AdvanceToNotWhiteSpace();
                value = new JsonSpan(name, ref providedReader);
                switch (providedReader.CurrentValue) {
                    case COMMA:
                    case RBRACE:
                    case RBRACKET:
                        break;
                    default: {
                        providedReader.AdvanceToCommaOrEndBrace();
                        break;
                    }
                }
            } while (!key.StartsWith(name));
            if (key.Length != value.GetKeyLen) {
                return value.TryGetKey(key.Slice(value.GetKeyLen + 1), out value);
            }
            return true;
        }

        public readonly Enumerator GetEnumerator() => new Enumerator(in this);

        /// <summary>
        /// Get the length of the reference area
        /// </summary>
        public readonly int GetLength => this.Value.Length;

        public readonly int GetKeyLen => this.Key.Length;

        /// <summary>
        /// Get the string value as-is in relation to this object
        /// </summary>
        public readonly string GetStringLiteral => this.Value.ToString();

        /// <summary>
        /// Get the decoded string value of the object
        /// </summary>
        public readonly string GetStringDecoded => GetStringDecodedFromSpan(in this.Value);

        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly string TryGetString(in ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(in key, out JsonSpan value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetString(in ReadOnlySpan<char> key, out string @out, bool decoded = true) {
            if (this.TryGetKey(key, out JsonSpan value) && value.Type == JsonType.String) {
                @out = decoded ? value.GetStringDecoded : value.GetStringLiteral;
                return true;
            }
            else {
                @out = string.Empty;
                return false;
            }
        }

        public override readonly string ToString() => this.ToString(Default_ToStringFormat);

        public readonly string ToString(ToStringFormat format) {
            int indent = 0;
            int pos = 0;

            bool pretty = HasFlag((long)format, (long)ToStringFormat.Pretty);
            bool translateUnicode = HasFlag((long)format, (long)ToStringFormat.TranslateUnicode);
            bool lowerCaseBool = HasFlag((long)format, (long)ToStringFormat.LowerCaseBool);
            bool reparseNumbers = HasFlag((long)format, (long)ToStringFormat.ReParseNumbers);

            char[] buffer = ArrayPool<char>.Shared.Rent(this.GetLength * INDENT_LEN + 64); // 0 returns empty array?
            this.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref buffer, ref indent, ref pos, INDENT_TABS.AsSpan());
            string builtString = buffer.AsSpan().Slice(0, pos).ToString();
            ArrayPool<char>.Shared.Return(buffer);
            return builtString;
        }

        /// <summary>
        /// Recursive method to build the json ToString output
        /// </summary>
        private readonly void ProcessString(bool AsValue, in bool pretty, in bool translateUnicode, in bool lowerCaseBool, in bool reparseNumbers, ref char[] sb, ref int indent, ref int sbPos, in ReadOnlySpan<char> indentSpan) {
            switch (this.Type) {
                case JsonType.String: {
                    EnsureBufferCapacity(sbPos + this.GetLength + 2, ref sb);
                    sb[sbPos++] = '"';
                    if (translateUnicode) {
                        TranslateUnicodeIntoBufferFromSpan(this.Value, sb.AsSpan(), ref sbPos);
                    }
                    else {
                        ReadOnlySpan<char> refSpan = this.Value;
                        refSpan.CopyTo(sb.AsSpan(sbPos, this.GetLength));
                        sbPos += this.GetLength;
                    }
                    sb[sbPos++] = '"';
                    return;
                }
                case JsonType.Null: {
                    EnsureBufferCapacity(sbPos + 4, ref sb);
                    this.Value.CopyTo(sb.AsSpan(sbPos, 4));
                    sbPos += 4;
                    return;
                }
                case JsonType.Number: {
                    if (reparseNumbers) {
                        EnsureBufferCapacity(sbPos + 32, ref sb);
                        this.GetNumber.TryFormat(sb.AsSpan(sbPos), out int refSpanLen);
                        sbPos += refSpanLen;
                    }
                    else {
                        EnsureBufferCapacity(sbPos + this.GetLength, ref sb);
                        this.Value.CopyTo(sb.AsSpan(sbPos, this.GetLength));
                        sbPos += this.GetLength;
                    }
                    return;
                }
                case JsonType.Boolean: {
                    EnsureBufferCapacity(sbPos + 5, ref sb);
                    this.Value.CopyTo(sb.AsSpan(sbPos, this.GetLength));
                    if (lowerCaseBool) {
                        switch (this.Value[0]) {
                            case 'F':
                                sb[sbPos] = 'f';
                                break;
                            case 'T':
                                sb[sbPos] = 't';
                                break;
                        }
                    }
                    sbPos += this.GetLength;
                    return;
                }
                case JsonType.Object: {
                    int x;
                    int y;
                    if (pretty && !AsValue) {
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                        for (x = indent; --x >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                    }

                    EnsureBufferCapacity(sbPos + 4, ref sb);
                    sb[sbPos++] = '{';
                    if (this.IsEmpty) {
                        if (pretty) {
                            sb[sbPos++] = ' ';
                        }
                        sb[sbPos++] = '}';
                        return;
                    }

                    JsonSpan prior = Empty;
                    JsonSpan current = Empty;
                    if (pretty) {
                        indent++;
                        sb[sbPos++] = '\n';
                        foreach (JsonSpan next in this) {
                            current = next;
                            if (!prior.IsNothing) {
                                EnsureBufferCapacity(sbPos + (INDENT_LEN * indent) + prior.GetKeyLen + 4, ref sb);
                                for (y = indent; --y >= 0;) {
                                    indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                                    sbPos += INDENT_LEN;
                                }
                                sb[sbPos++] = '"';
                                prior.Key.CopyTo(sb.AsSpan(sbPos, prior.GetKeyLen));
                                sbPos += prior.GetKeyLen;
                                sb[sbPos++] = '"';
                                sb[sbPos++] = ':';
                                sb[sbPos++] = ' ';
                                prior.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                                EnsureBufferCapacity(sbPos + 2, ref sb);
                                sb[sbPos++] = ',';
                                sb[sbPos++] = '\n';
                            }
                            prior = current;
                        }
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent) + current.GetKeyLen + 4, ref sb);
                        for (y = indent; --y >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                        sb[sbPos++] = '"';
                        current.Key.CopyTo(sb.AsSpan(sbPos, current.GetKeyLen));
                        sbPos += current.GetKeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        current.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent) + 1, ref sb);
                        sb[sbPos++] = '\n';

                        indent--;
                        for (x = indent; --x >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                    }
                    else {
                        foreach (JsonSpan next in this) {
                            current = next;
                            if (!prior.IsNothing) {
                                EnsureBufferCapacity(sbPos + prior.GetKeyLen + 4, ref sb);
                                sb[sbPos++] = '"';
                                prior.Key.CopyTo(sb.AsSpan(sbPos, prior.GetKeyLen));
                                sbPos += prior.GetKeyLen;
                                sb[sbPos++] = '"';
                                sb[sbPos++] = ':';
                                sb[sbPos++] = ' ';
                                prior.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                                EnsureBufferCapacity(sbPos + 1, ref sb);
                                sb[sbPos++] = ',';
                            }
                            prior = current;
                        }
                        EnsureBufferCapacity(sbPos + current.GetKeyLen + 4, ref sb);
                        sb[sbPos++] = '"';
                        current.Key.CopyTo(sb.AsSpan(sbPos, current.GetKeyLen));
                        sbPos += current.GetKeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        current.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                    }

                    EnsureBufferCapacity(sbPos + 1, ref sb);
                    sb[sbPos++] = '}';
                    return;
                }
                case JsonType.Array: {
                    int x;
                    int y;
                    if (pretty && !AsValue) {
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                        for (x = indent; --x >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                    }
                    EnsureBufferCapacity(sbPos + 4, ref sb);
                    sb[sbPos++] = '[';
                    if (this.IsEmpty) {
                        if (pretty) {
                            sb[sbPos++] = ' ';
                        }
                        sb[sbPos++] = ']';
                        return;
                    }

                    JsonSpan prior = Empty;
                    JsonSpan current = Empty;
                    if (pretty) {
                        indent++;
                        sb[sbPos++] = '\n';
                        foreach (JsonSpan next in this) {
                            current = next;
                            if (!prior.IsNothing) {
                                if (HasFlag((long)JsonType.Value, (long)prior.Type)) {
                                    EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                                    for (y = indent; --y >= 0;) {
                                        indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                                        sbPos += INDENT_LEN;
                                    }
                                }
                                prior.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                                EnsureBufferCapacity(sbPos + 2, ref sb);
                                sb[sbPos++] = ',';
                                sb[sbPos++] = '\n';
                            }
                            prior = current;
                        }
                        if (HasFlag((long)JsonType.Value, (long)current.Type)) {
                            EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                            for (y = indent; --y >= 0;) {
                                indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                                sbPos += INDENT_LEN;
                            }
                        }
                        current.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent) + 1, ref sb);
                        sb[sbPos++] = '\n';

                        indent--;
                        for (x = indent; --x >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                    }
                    else {
                        foreach (JsonSpan next in this) {
                            current = next;
                            if (!prior.IsNothing) {
                                prior.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                                EnsureBufferCapacity(sbPos + 1, ref sb);
                                sb[sbPos++] = ',';
                            }
                            prior = current;
                        }
                        current.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                    }

                    EnsureBufferCapacity(sbPos + 1, ref sb);
                    sb[sbPos++] = ']';
                    return;
                }
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
                if (this.GetLength > 0 && HasFlag((long)this.Type, (long)JsonType.Number)) {
                    int len = this.GetLength;
                    int pos = 0;

                    // handle sign
                    char c = this.Value[pos];
                    bool negative = c == '-';
                    if (negative || c == '+') {
                        pos++;
                    }

                    // find 'e' or 'E' for exponent (if any)
                    int eIndex = -1;
                    for (int x = pos; x < len; x++) {
                        c = this.Value[x];
                        if (c == 'e' || c == 'E') {
                            eIndex = x;
                            break;
                        }
                    }

                    // main part (integer + fractional) excludes exponent
                    ReadOnlySpan<char> mainSpan = eIndex > 0 ? this.Value.Slice(pos, eIndex - pos) : this.Value.Slice(pos);

                    // parse integer and fractional parts precisely
                    double value = 0.0d;
                    bool seenDecimal = false;
                    double fracDiv = 1.0;
                    for (int x = 0; x < mainSpan.Length; x++) {
                        c = mainSpan[x];
                        if (c == '.') {
                            seenDecimal = true;
                            continue;
                        }
                        int digit = ReadHexNumber(c); // expects '0'..'9'
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
                            char ec = this.Value[expPos];
                            expNegative = ec == '-';
                            if (expNegative || ec == '+') {
                                expPos++;
                            }
                        }
                        int expVal = 0;
                        for (int x = expPos; x < len; x++) {
                            expVal = expVal * 10 + ReadHexNumber(this.Value[x]);
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
        }

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
        public double TryGetNumber(in ReadOnlySpan<char> key) => this.TryGetNumber(in key, out double value) ? value : double.NaN;
        /// <summary>
        /// Try to get the numerical value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetNumber(in ReadOnlySpan<char> key, out double @out) {
            if (this.TryGetKey(in key, out JsonSpan value) && value.Type == JsonType.Number) {
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
        public readonly bool TryGetNumber<T>(in ReadOnlySpan<char> key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
            if (this.TryGetKey(in key, out JsonSpan value) && value.Type == JsonType.Number) {
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
        public readonly bool TryGetBool(string key) => this.TryGetKey(key, out JsonSpan value) && value.GetBool;

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
        public readonly bool TryGetBool(in ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(in key, out JsonSpan value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly DateTime GetDateTime => DateTime.TryParse(this.Value, out DateTime value) ? value : DateTime.MinValue;

        /// <summary>
        /// Gets this value as a System.DateTime using TryParse
        /// </summary>
        public readonly DateTime AsDateTime => JsonType.DateTime.HasFlag(this.Type) && DateTime.TryParse(this.Value, out DateTime value) ? value : DateTime.MinValue;

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
        public readonly bool TryGetDateTime(in ReadOnlySpan<char> key, out DateTime @out) {
            if (this.TryGetKey(in key, out JsonSpan value) && HasFlag((long)JsonType.DateTime, (long)this.Type)) {
                @out = value.GetDateTime;
                return true;
            }
            else {
                @out = DateTime.MinValue;
                return false;
            }
        }

        /// <summary>
        /// Get if This object is Null
        /// </summary>
        public readonly bool IsNull => this.Type == JsonType.Null;

    }

    #endregion

    #region ### JsonMemory ###

    [StructLayout(LayoutKind.Sequential)]
    public readonly struct JsonMemory : IEquatable<JsonMemory>, IComparable<JsonMemory>, IDisposable {

        private readonly ReadOnlyMemory<char> ReferenceData;
        private readonly ReadOnlyMemory<char> KeyData;
        public readonly JsonType Type;
        private readonly IEnumerable<JsonMemory> InnerValues;
        /// <summary>
        /// Gets the length of the contained values for Array or Object
        /// </summary>
        public readonly int InnerLength;


        private JsonMemory(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> reference, ref JsonReader reader, ref JsonMemory[] existingBuffer, int bufferIndex) {
            if (reference.IsEmpty) {
                this = Empty;
                this.KeyData = key;
                return;
            }
            this.KeyData = key;
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
                    this.InnerValues = Array.Empty<JsonMemory>();
                    int left = reader.CurrentIndex + 1;
                    int right = reader.AdvanceTo(QUOTE);

                    if ((right ^ left) == 0) {
                        this.ReferenceData = ReadOnlyMemory<char>.Empty;
                    }
                    else {
                        this.ReferenceData = reference.Slice(left, right - left);
                    }
                    this.InnerLength = 0;
                    return;
                }
                case LBRACKET: {
                    this.Type = JsonType.Array;
                    int left = reader.CurrentIndex;
                    if (reader.AdvanceToNotWhiteSpace() == RBRACKET) {
                        this.InnerValues = Array.Empty<JsonMemory>();
                        this.ReferenceData = reference.Slice(left, reader.CurrentIndex - left + 1);
                        this.InnerLength = 0;
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
                        JsonContainerPool.EnsureBufferCapacity(bufferIndex + 1, ref existingBuffer);
                    }

                    int bufPos = bufferIndex;
                    int bTemp;

                    JsonMemory newValue;
                    while (true) {
                        left = reader.CurrentIndex;
                        ushort leftChar = reader.CurrentValue;

                        switch (leftChar) {
                            case QUOTE: {
                                reader.AdvanceTo(QUOTE);
                                int r = reader.CurrentIndex + 1;
                                ushort advance = reader.AdvanceToCommaOrEndBrace();
                                bTemp = bufPos;
                                newValue = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.String, reference.Slice(left, r - left));
                                ++bufPos;
                                JsonContainerPool.EnsureBufferCapacity(bufPos, ref existingBuffer);
                                existingBuffer[bTemp] = newValue;
                                if (advance == COMMA) {
                                    reader.AdvanceToNotWhiteSpace();
                                    continue;
                                }
                                else if (advance == RBRACKET) {
                                    goto ReadComplete;
                                }
                                throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                            }
                            case LBRACE:
                            case LBRACKET: {
                                bTemp = bufPos;
                                newValue = new JsonMemory(ReadOnlyMemory<char>.Empty, in reference, ref reader, ref existingBuffer, ++bufPos);
                                JsonContainerPool.EnsureBufferCapacity(bufPos, ref existingBuffer);
                                existingBuffer[bTemp] = newValue;
                                if (reader.AdvanceToCommaOrEndBrace() == RBRACKET) {
                                    goto ReadComplete;
                                }
                                reader.AdvanceToNotWhiteSpace();
                                continue;
                            }
                            default: {
                                ushort advanced = reader.AdvanceToCommaOrEndBrace();
                                int continuation = reader.CurrentIndex + 1;
                                reader.RetreatToNotWhiteSpace();
                                reader.Increment();
                                bTemp = bufPos;
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
                                                        newValue = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Null, NULL.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
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
                                                        newValue = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Boolean, bool.TrueString.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
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
                                                            newValue = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Boolean, bool.FalseString.AsMemory());
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                                    }
                                    default: {
                                        ReadOnlyMemory<char> numberArea = reference.Slice(left, reader.CurrentIndex - left);
                                        if (IsNumber(numberArea.Span)) {
                                            newValue = new JsonMemory(ReadOnlyMemory<char>.Empty, JsonType.Number, numberArea);
                                            break;
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                                    }
                                }

                                ++bufPos;
                                JsonContainerPool.EnsureBufferCapacity(bufPos, ref existingBuffer);
                                existingBuffer[bTemp] = newValue;
                                if (advanced == RBRACKET) {
                                    goto ReadComplete;
                                }
                                reader.SetIndexPosition(continuation);
                                reader.AdvanceToNotWhiteSpace();
                                continue;
                            }
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                    ReadComplete:
                    int valuesLen = bufPos - bufferIndex;
                    this.InnerLength = valuesLen;
                    JsonContainerPool.RentedContainer rented = new JsonContainerPool.RentedContainer(valuesLen);
                    existingBuffer.AsSpan(bufferIndex, valuesLen).CopyTo(rented.Value.AsSpan());
                    this.InnerValues = rented;
                    if (bufferSource) {
                        JsonContainerPool.Return(existingBuffer);
                    }
                    this.ReferenceData = reference.Slice(refStart, reader.CurrentIndex - refStart + 1);
                    return;
                }
                case LBRACE: {
                    this.Type = JsonType.Object;
                    int left = reader.CurrentIndex;
                    if (reader.AdvanceToNotWhiteSpace() == RBRACE) {
                        this.InnerValues = Array.Empty<JsonMemory>();
                        this.ReferenceData = reference.Slice(left, reader.CurrentIndex - left + 1);
                        this.InnerLength = 0;
                        return;
                    }
                    int refStart = left;

                    bool bufferSource = false;
                    if (existingBuffer == null) {
                        existingBuffer = JsonContainerPool.Rent(16);
                        bufferSource = true;
                    }
                    else {
                        JsonContainerPool.EnsureBufferCapacity(bufferIndex + 1, ref existingBuffer);
                    }

                    int bufPos = bufferIndex;
                    int bTemp;
                    int nameL;
                    int nameR;
                    JsonMemory newValue;

                    while (true) {
                        if (reader.CurrentValue != QUOTE) {
                            reader.AdvanceTo(QUOTE);
                        }
                        nameL = reader.CurrentIndex + 1;
                        reader.AdvanceTo(QUOTE);
                        nameR = reader.CurrentIndex - nameL;
                        if (nameR == 1) {
                            nameL = 0;
                            nameR = 0;
                        }
                        do {
                            reader.Increment();
                        } while (reader.CurrentValue == COLON);
                        reader.AdvanceToNotWhiteSpace();
                        left = reader.CurrentIndex;
                        ushort leftChar = reader.CurrentValue;

                        switch (leftChar) {
                            case QUOTE: {
                                left++;
                                reader.AdvanceTo(QUOTE);
                                int r = reader.CurrentIndex;
                                ushort advance = reader.AdvanceToCommaOrEndBrace();
                                bTemp = bufPos;
                                newValue = new JsonMemory(reference.Slice(nameL, nameR), JsonType.String, reference.Slice(left, r - left));
                                ++bufPos;
                                JsonContainerPool.EnsureBufferCapacity(bufPos, ref existingBuffer);
                                existingBuffer[bTemp] = newValue;
                                if (advance == COMMA) {
                                    reader.Increment();
                                    goto NextObject;
                                }
                                else if (advance == RBRACE) {
                                    goto ReadComplete;
                                }
                                throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                            }
                            case LBRACE:
                            case LBRACKET: {
                                bTemp = bufPos;
                                newValue = new JsonMemory(reference.Slice(nameL, nameR), in reference, ref reader, ref existingBuffer, ++bufPos);
                                JsonContainerPool.EnsureBufferCapacity(bufPos, ref existingBuffer);
                                existingBuffer[bTemp] = newValue;
                                switch (reader.AdvanceToCommaOrEndBrace()) {
                                    case RBRACKET:
                                        goto ReadComplete;
                                    case COMMA:
                                        continue;
                                }
                                continue;
                            }
                            default: {
                                ushort advanced = reader.AdvanceToCommaOrEndBrace();
                                int continuation = reader.CurrentIndex + 1;
                                reader.RetreatToNotWhiteSpace();
                                reader.Increment();
                                bTemp = bufPos;
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
                                                        newValue = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Null, NULL.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
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
                                                        newValue = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Boolean, bool.TrueString.AsMemory());
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
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
                                                            newValue = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Boolean, bool.FalseString.AsMemory());
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                                    }
                                    default: {
                                        ReadOnlyMemory<char> numberArea = reference.Slice(left, reader.CurrentIndex - left);
                                        if (IsNumber(numberArea.Span)) {
                                            newValue = new JsonMemory(reference.Slice(nameL, nameR), JsonType.Number, numberArea);
                                            break;
                                        }
                                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                                    }

                                }
                                ++bufPos;
                                JsonContainerPool.EnsureBufferCapacity(bufPos, ref existingBuffer);
                                existingBuffer[bTemp] = newValue;
                                if (advanced == RBRACE) {
                                    goto ReadComplete;
                                }
                                reader.SetIndexPosition(continuation);
                                continue;
                            }
                        }
                        throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                        NextObject:
                        continue;
                    }

                    ReadComplete:
                    int valuesLen = bufPos - bufferIndex;
                    this.InnerLength = valuesLen;
                    JsonContainerPool.RentedContainer rented = new JsonContainerPool.RentedContainer(valuesLen);
                    existingBuffer.AsSpan(bufferIndex, valuesLen).CopyTo(rented.Value.AsSpan());
                    this.InnerValues = rented;
                    if (bufferSource) {
                        JsonContainerPool.Return(existingBuffer);
                    }
                    this.ReferenceData = reference.Slice(refStart, reader.CurrentIndex - refStart + 1);
                    return;
                }
                case N_LOWER:
                case N_UPPER: {
                    this.InnerLength = 0;
                    this.Type = JsonType.Null;
                    this.InnerValues = Array.Empty<JsonMemory>();
                    this.ReferenceData = NULL.AsMemory();

                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CurrentIndex - left;
                    if (right == 4) {
                        ReadOnlySpan<char> data = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, right));
                        if (this.ReferenceData.Span.Equals(data, StringComparison.OrdinalIgnoreCase)) {
                            return;
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                }
                case T_LOWER:
                case T_UPPER: {
                    this.InnerLength = 0;
                    this.Type = JsonType.Boolean;
                    this.InnerValues = Array.Empty<JsonMemory>();
                    this.ReferenceData = bool.TrueString.AsMemory();

                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CurrentIndex - left;
                    if (right == 4) {
                        ReadOnlySpan<char> data = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, right));
                        if (this.ReferenceData.Span.Equals(data, StringComparison.OrdinalIgnoreCase)) {
                            return;
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                }
                case F_LOWER:
                case F_UPPER: {
                    this.InnerLength = 0;
                    this.Type = JsonType.Boolean;
                    this.InnerValues = Array.Empty<JsonMemory>();
                    this.ReferenceData = bool.FalseString.AsMemory();

                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CurrentIndex - left;
                    if (right == 5) {
                        ReadOnlySpan<char> data = MemoryMarshal.Cast<ushort, char>(reader.Slice(left, right));
                        if (this.ReferenceData.Span.Equals(data, StringComparison.OrdinalIgnoreCase)) {
                            return;
                        }
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {reference.ToString()}, Position: {reader.CurrentIndex})", nameof(reference));
                }
                default: {
                    this.InnerLength = 0;
                    int left = reader.CurrentIndex;
                    reader.AdvanceToValueEnding();
                    int right = reader.CurrentIndex;
                    this.Type = JsonType.Number;
                    this.InnerValues = Array.Empty<JsonMemory>();

                    this.ReferenceData = reference.Slice(left, right - left);
                    if (IsNumber(this.ReferenceData.Span)) {
                        return;
                    }
                    throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {this.ReferenceData.ToString()})", nameof(reference));
                }
            }

            throw new ArgumentException($"Parse failed (TryParse: {reference.ToString()})", nameof(reference));
        }
        private JsonMemory(in JsonType type, bool rented, IEnumerable<JsonMemory> contents, int innerLength = -1) : this(ReadOnlyMemory<char>.Empty, in type, rented, contents, innerLength) { }

        private JsonMemory(in ReadOnlyMemory<char> key, in JsonType type, bool rented, IEnumerable<JsonMemory> contents, int innerLength = -1) {
            switch (type) {
                case JsonType.Array:
                case JsonType.Object:
                    this.Type = type;
                    this.InnerValues = contents;
                    if (innerLength >= 0) {
                        this.InnerLength = innerLength;
                    }
                    else {
                        if (contents is JsonContainerPool.RentedContainer rc) {
                            this.InnerLength = rc.Value.Length;
                        }
                        else if (contents is JsonMemory[] ar) {
                            this.InnerLength = ar.Length;
                        }
                        else {
                            throw new InvalidCastException("Unsupported container type, only JsonMemory[] or JsonContainerPool.RentedContainer supported");
                        }
                    }
                    this.KeyData = key;
                    this.ReferenceData = ReadOnlyMemory<char>.Empty;
                    break;
                default:
                    throw new NotSupportedException();
            }
        }

        private JsonMemory(in ReadOnlyMemory<char> key, in JsonMemory value) {
            this.KeyData = key;
            this.Type = value.Type;
            this.ReferenceData = value.ReferenceData;
            this.InnerLength = value.InnerLength;
            if (value.InnerValues is JsonContainerPool.RentedContainer rc) { // Dispose protection for the new value
                JsonContainerPool.RentedContainer newConatiner = new JsonContainerPool.RentedContainer(value.InnerLength);
                rc.Value.AsSpan(0, value.InnerLength).CopyTo(newConatiner.Value.AsSpan(0, value.InnerLength));
                this.InnerValues = newConatiner;
            }
            else {
                this.InnerValues = value.InnerValues;
            }
        }

        private JsonMemory(in ReadOnlyMemory<char> key, bool value) {
            this.KeyData = key;
            this.Type = JsonType.Boolean;
            this.ReferenceData = value ? bool.TrueString.AsMemory() : bool.FalseString.AsMemory();
            this.InnerValues = Array.Empty<JsonMemory>();
            this.InnerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key, double value) {
            this.KeyData = key;
            this.Type = JsonType.Number;
            this.ReferenceData = value.ToString().AsMemory();
            this.InnerValues = Array.Empty<JsonMemory>();
            this.InnerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key) {
            this.KeyData = key;
            this.Type = JsonType.Null;
            this.ReferenceData = NULL.AsMemory();
            this.InnerValues = Array.Empty<JsonMemory>();
            this.InnerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key, in string value) : this(key, value.AsMemory()) { }
        private JsonMemory(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> value) {
            this.KeyData = key;
            this.Type = JsonType.String;
            this.ReferenceData = value;
            this.InnerValues = Array.Empty<JsonMemory>();
            this.InnerLength = 0;
        }

        private JsonMemory(in ReadOnlyMemory<char> key, JsonType overridingType, in ReadOnlyMemory<char> value) {
            if (HasFlag((long)JsonType.Container, (long)overridingType)) {
                throw new ArgumentException("A container cannot be set with this override", nameof(overridingType));
            }
            this.KeyData = key;
            this.Type = overridingType;
            this.ReferenceData = value;
            this.InnerValues = Array.Empty<JsonMemory>();
            this.InnerLength = 0;
        }

        public void Dispose() => this.Dispose(true);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="includingSubObjects">If this object will dispose everything or just itself</param>
        public void Dispose(bool includingSubObjects = true) {
            static void CycleEach(in JsonMemory container) {
                for (int x = 0; x < container.InnerLength; x++) {
                    ref readonly JsonMemory next = ref container[x];
                    if (HasFlag((long)JsonType.Container, (long)next.Type)) {
                        CycleEach(in next);
                        if (next.InnerValues is JsonContainerPool.RentedContainer rc) { // Return after cycled
                            rc.Dispose();
                        }
                    }
                }
            }

            if (includingSubObjects) {
                CycleEach(in this);
            }
            if (this.InnerValues is JsonContainerPool.RentedContainer rc) {
                rc.Dispose();
            }
        }

        /// <summary>
        /// If this was created using rented space, making it eligible within using or calling dispose to save memory
        /// </summary>
        public readonly bool CanDispose => this.InnerValues is JsonContainerPool.RentedContainer;

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
                if (this.Type == JsonType.Object) {
                    int hash = ComputeHash(in key, out int pathLen);
                    for (int x = this.InnerLength; --x >= 0;) {
                        ref readonly JsonMemory value = ref this.GetInsideValues[x];
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
                switch (this.Type) {
                    case JsonType.Object:
                    case JsonType.Array:
                        return ref this.GetInsideValues[index];
                    default:
                        throw new IndexOutOfRangeException();
                }
            }
        }

        private readonly int KeyHash => ComputeHash(this.KeyData.Span, out _);
        private readonly int KeyLen => this.GetKeyAsSpan.Length;

        public readonly override string ToString() => this.ToString(Default_ToStringFormat);

        public readonly string ToString(ToStringFormat format) {
            static int RecursiveCount(in JsonMemory value) {
                int counter = value.InnerLength;
                for (int x = 0; x < value.InnerLength; x++) {
                    counter += RecursiveCount(value[x]);
                }
                return counter;
            }

            int indent = 0;
            int pos = 0;

            bool pretty = HasFlag((long)format, (long)ToStringFormat.Pretty);
            bool translateUnicode = HasFlag((long)format, (long)ToStringFormat.TranslateUnicode);
            bool lowerCaseBool = HasFlag((long)format, (long)ToStringFormat.LowerCaseBool);
            bool reparseNumbers = HasFlag((long)format, (long)ToStringFormat.ReParseNumbers);

            int objectCount = RecursiveCount(this);
            int estimatedCapacity = this.GetLength + objectCount * 5 + (pretty ? (objectCount * INDENT_LEN * 2) : 0) + 64; //  Reference Len + number of items if all where considered KeyValues + potential needed indents + extra buffer for additional characters

            char[] buffer = ArrayPool<char>.Shared.Rent(estimatedCapacity); // 0 returns empty array?
            this.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref buffer, ref indent, ref pos, INDENT_TABS.AsSpan());
            string builtString = buffer.AsSpan().Slice(0, pos).ToString();
            ArrayPool<char>.Shared.Return(buffer);
            return builtString;
        }

        /// <summary>
        /// Recursive method to build the json ToString output
        /// </summary>
        private readonly void ProcessString(bool AsValue, in bool pretty, in bool translateUnicode, in bool lowerCaseBool, in bool reparseNumbers, ref char[] sb, ref int indent, ref int sbPos, in ReadOnlySpan<char> indentSpan) {
            switch (this.Type) {
                case JsonType.String: {
                    EnsureBufferCapacity(sbPos + this.GetLength + 2, ref sb);
                    sb[sbPos++] = '"';
                    if (translateUnicode) {
                        TranslateUnicodeIntoBufferFromSpan(this.GetValueAsSpan, sb.AsSpan(), ref sbPos);
                    }
                    else {
                        ReadOnlySpan<char> refSpan = this.GetValueAsSpan;
                        refSpan.CopyTo(sb.AsSpan(sbPos, this.GetLength));
                        sbPos += this.GetLength;
                    }
                    sb[sbPos++] = '"';
                    return;
                }
                case JsonType.Null: {
                    EnsureBufferCapacity(sbPos + 4, ref sb);
                    this.GetValueAsSpan.CopyTo(sb.AsSpan(sbPos, 4));
                    sbPos += 4;
                    return;
                }
                case JsonType.Number: {
                    if (reparseNumbers) {
                        EnsureBufferCapacity(sbPos + 32, ref sb);
                        this.GetNumber.TryFormat(sb.AsSpan(sbPos), out int refSpanLen);
                        sbPos += refSpanLen;
                    }
                    else {
                        ReadOnlySpan<char> numberSpan = this.GetValueAsSpan;
                        EnsureBufferCapacity(sbPos + this.GetLength, ref sb);
                        numberSpan.CopyTo(sb.AsSpan(sbPos, this.GetLength));
                        sbPos += this.GetLength;
                    }
                    return;
                }
                case JsonType.Boolean: {
                    ReadOnlySpan<char> refSpan = this.GetValueAsSpan;
                    int len = this.GetLength;
                    EnsureBufferCapacity(sbPos + 5, ref sb);
                    if (lowerCaseBool) {
                        // write lowercase first char
                        char first = refSpan[0];
                        if (first == 'T') {
                            sb[sbPos++] = 't';
                        }
                        else if (first == 'F') {
                            sb[sbPos++] = 'f';
                        }
                        else {
                            sb[sbPos++] = first;
                        }
                        refSpan.Slice(1).CopyTo(sb.AsSpan(sbPos, --len));
                        sbPos += len;
                    }
                    else {
                        refSpan.CopyTo(sb.AsSpan(sbPos, len));
                        sbPos += len;
                    }
                    return;
                }
                case JsonType.Object: {
                    int x;
                    int y;
                    if (pretty && !AsValue) {
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                        for (x = indent; --x >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                    }

                    EnsureBufferCapacity(sbPos + 4, ref sb);
                    sb[sbPos++] = '{';

                    if (this.InnerLength == 0) {
                        if (pretty) {
                            sb[sbPos++] = ' ';
                        }
                        sb[sbPos++] = '}';
                        return;
                    }
                    int limit = this.InnerLength - 1;
                    if (pretty) {
                        indent++;
                        sb[sbPos++] = '\n';
                        for (x = 0; x < limit; x++) {
                            ref readonly JsonMemory value = ref this.GetInsideValues[x];
                            EnsureBufferCapacity(sbPos + (INDENT_LEN * indent) + value.KeyLen + 4, ref sb);
                            for (y = indent; --y >= 0;) {
                                indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                                sbPos += INDENT_LEN;
                            }
                            sb[sbPos++] = '"';
                            value.GetKeyAsSpan.CopyTo(sb.AsSpan(sbPos, value.KeyLen));
                            sbPos += value.KeyLen;
                            sb[sbPos++] = '"';
                            sb[sbPos++] = ':';
                            sb[sbPos++] = ' ';
                            value.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                            EnsureBufferCapacity(sbPos + 2, ref sb);
                            sb[sbPos++] = ',';
                            sb[sbPos++] = '\n';
                        }
                        ref readonly JsonMemory valueLast = ref this.GetInsideValues[x];
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent) + valueLast.KeyLen + 4, ref sb);
                        for (y = indent; --y >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                        sb[sbPos++] = '"';
                        valueLast.GetKeyAsSpan.CopyTo(sb.AsSpan(sbPos, valueLast.KeyLen));
                        sbPos += valueLast.KeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        valueLast.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                        sb[sbPos++] = '\n';

                        indent--;
                        for (x = indent; --x >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                    }
                    else {
                        for (x = 0; x < limit; x++) {
                            ref readonly JsonMemory value = ref this.GetInsideValues[x];
                            EnsureBufferCapacity(sbPos + value.KeyLen + 4, ref sb);
                            sb[sbPos++] = '"';
                            value.GetKeyAsSpan.CopyTo(sb.AsSpan(sbPos, value.KeyLen));
                            sbPos += value.KeyLen;
                            sb[sbPos++] = '"';
                            sb[sbPos++] = ':';
                            sb[sbPos++] = ' ';
                            value.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                            EnsureBufferCapacity(sbPos + 1, ref sb);
                            sb[sbPos++] = ',';
                        }
                        ref readonly JsonMemory valueLast = ref this.GetInsideValues[x];
                        EnsureBufferCapacity(sbPos + valueLast.KeyLen + 4, ref sb);
                        sb[sbPos++] = '"';
                        valueLast.GetKeyAsSpan.CopyTo(sb.AsSpan(sbPos, valueLast.KeyLen));
                        sbPos += valueLast.KeyLen;
                        sb[sbPos++] = '"';
                        sb[sbPos++] = ':';
                        sb[sbPos++] = ' ';
                        valueLast.ProcessString(true, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                    }

                    EnsureBufferCapacity(sbPos + 1, ref sb);
                    sb[sbPos++] = '}';
                    return;
                }
                case JsonType.Array: {
                    int x;
                    int y;
                    if (pretty && !AsValue) {
                        EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                        for (x = indent; --x >= 0;) {
                            indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                            sbPos += INDENT_LEN;
                        }
                    }
                    EnsureBufferCapacity(sbPos + 4, ref sb);
                    sb[sbPos++] = '[';

                    if (this.InnerLength == 0) {
                        if (pretty) {
                            sb[sbPos++] = ' ';
                        }
                        sb[sbPos++] = ']';
                    }
                    else {
                        int limit = this.InnerLength - 1;
                        if (pretty) {
                            indent++;
                            sb[sbPos++] = '\n';
                            for (x = 0; x < limit; x++) {
                                ref readonly JsonMemory value = ref this.GetInsideValues[x];
                                if (HasFlag((long)JsonType.Value, (long)value.Type)) {
                                    EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                                    for (y = indent; --y >= 0;) {
                                        indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                                        sbPos += INDENT_LEN;
                                    }
                                }
                                value.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                                EnsureBufferCapacity(sbPos + 2, ref sb);
                                sb[sbPos++] = ',';
                                sb[sbPos++] = '\n';
                            }
                            ref readonly JsonMemory valueLast = ref this.GetInsideValues[x];
                            if (HasFlag((long)JsonType.Value, (long)valueLast.Type)) {
                                EnsureBufferCapacity(sbPos + (INDENT_LEN * indent), ref sb);
                                for (y = indent; --y >= 0;) {
                                    indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                                    sbPos += INDENT_LEN;
                                }
                            }
                            valueLast.ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                            EnsureBufferCapacity(sbPos + (INDENT_LEN * indent) + 1, ref sb);
                            sb[sbPos++] = '\n';

                            indent--;
                            for (x = indent; --x >= 0;) {
                                indentSpan.CopyTo(sb.AsSpan(sbPos, INDENT_LEN));
                                sbPos += INDENT_LEN;
                            }
                        }
                        else {
                            for (x = 0; x < limit; x++) {
                                this.GetInsideValues[x].ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                                EnsureBufferCapacity(sbPos + 1, ref sb);
                                sb[sbPos++] = ',';
                            }
                            this.GetInsideValues[x].ProcessString(false, in pretty, in translateUnicode, in lowerCaseBool, in reparseNumbers, ref sb, ref indent, ref sbPos, in indentSpan);
                        }

                        EnsureBufferCapacity(sbPos + 1, ref sb);
                        sb[sbPos++] = ']';
                    }
                    return;
                }
            }
        }

        /// <summary>
        /// Get the length of the reference area or if no reference is available, the combined internal GetLength values
        /// </summary>
        public readonly int GetLength
        {
            get
            {
                if (this.ReferenceData.IsEmpty) {
                    int len = 0;
                    for (int x = this.InnerLength; --x >= 0;) {
                        len += this.GetInsideValues[x].GetLength;
                    }
                    return len;
                }
                else {
                    return this.ReferenceData.Length;
                }
            }
        }

        /// <summary>
        /// Get the literal string value of the object
        /// </summary>
        public readonly string GetStringLiteral => this.ReferenceData.ToString();

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
        public readonly string TryGetString(in ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(key, out JsonMemory value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
        /// <summary>
        /// Try to get the string value of the object at path
        /// </summary>
        /// <param name="key"></param>
        /// <returns></returns>
        public readonly bool TryGetString(in ReadOnlySpan<char> key, out string @out, bool decoded = true) {
            if (this.TryGetKey(key, out JsonMemory value) && value.Type == JsonType.String) {
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
        public readonly string GetValue => this.Type switch { JsonType.Object => this.ToString(), JsonType.Array => this.ToString(), _ => this.ReferenceData.ToString() };

        /// <summary>
        /// Get the data used inside This object
        /// </summary>
        public readonly ReadOnlySpan<char> GetValueAsSpan => this.ReferenceData.Span;

        /// <summary>
        /// Get the data used inside This object
        /// </summary>
        public readonly ReadOnlyMemory<char> GetValueAsMemory => this.ReferenceData;

        /// <summary>
        /// Get the number contained inside This object
        /// </summary>
        public readonly double GetNumber
        {
            get
            {
                double returnNumber = double.NaN;
                if (HasFlag((long)this.Type, (long)JsonType.Number)) {
                    returnNumber = ParseNumber(this.GetValueAsSpan);
                }
                return returnNumber;
            }
        }

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
            if (this.TryGetKey(in key, out JsonMemory value) && value.Type == JsonType.Number) {
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
            if (this.TryGetKey(in key, out JsonMemory value) && value.Type == JsonType.Number) {
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
                if (this.InnerLength == 0) {
                    return Array.Empty<JsonMemory>();
                }
                else {
                    return this.GetInsideValues.ToArray();
                }
            }
        }

        /// <summary>
        /// Get the values contained inside This object
        /// </summary>
        public readonly ReadOnlySpan<JsonMemory> GetInsideValues
        {
            get
            {
                if (this.InnerValues is JsonContainerPool.RentedContainer rc) {
                    return (ReadOnlySpan<JsonMemory>)rc.Value.AsSpan(0, this.InnerLength);
                }
                else if (this.InnerValues is JsonMemory[] ar) {
                    return (ReadOnlySpan<JsonMemory>)ar.AsSpan(0, this.InnerLength);
                }
                else {
                    throw new InvalidCastException("Unsupported container type, only JsonMemory[] or JsonContainerPool.RentedContainer supported");
                }
            }
        }

        /// <summary>
        /// Get if This object is Null
        /// </summary>
        public readonly bool IsNull => this.Type == JsonType.Null;

        /// <summary>
        /// Get the bool value of This object
        /// </summary>
        public readonly bool GetBool => bool.TryParse(this.GetValueAsSpan, out bool value) && value;

        /// <summary>
        /// Get the inner values of this Object/Array as a string array
        /// </summary>
        public readonly string[] ToStringArray
        {
            get
            {
                string[] array = new string[this.InnerLength];
                for (int x = 0; x < this.InnerLength; x++) {
                    array[x] = this[x].GetStringDecoded;
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
        public readonly bool TryGetBool(in ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(in key, out JsonMemory value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

        /// <summary>
        /// Get the key of This object
        /// </summary>
        public readonly string GetKey => this.KeyData.IsEmpty ? string.Empty : this.KeyData.ToString();

        /// <summary>
        /// Get the key of This object
        /// </summary>
        public readonly ReadOnlySpan<char> GetKeyAsSpan => this.KeyData.IsEmpty ? ReadOnlySpan<char>.Empty : this.KeyData.Span;

        /// <summary>
        /// Get the key of This object
        /// </summary>
        public readonly ReadOnlyMemory<char> GetKeyAsMemory => this.KeyData.IsEmpty ? ReadOnlyMemory<char>.Empty : this.KeyData;

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
            if (this.Type == JsonType.Object) {
                int hash = ComputeHash(in key, out int pathLen);
                for (int x = this.InnerLength; --x >= 0;) {
                    ref readonly JsonMemory value = ref this.GetInsideValues[x];
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
            if (this.Type == JsonType.Object) {
                int hash = ComputeHash(in key, out int pathLen);
                for (int x = this.InnerLength; --x >= 0;) {
                    ref readonly JsonMemory value = ref this.GetInsideValues[x];
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

        public readonly ReadOnlySpan<JsonMemory>.Enumerator GetEnumerator() => this.GetInsideValues.GetEnumerator();

        public readonly override bool Equals(object obj) => obj is JsonMemory other && this.Equals(other);
        public readonly bool Equals(JsonMemory other) {
            if (this.Type.Equals(other.Type)
              && this.InnerValues.Equals(other.InnerValues)
              && this.KeyHash == other.KeyHash
              && this.GetValueAsSpan.Equals(other.GetValueAsSpan, StringComparison.Ordinal)) {
                return true;
            }
            return false;
        }
        public static bool operator ==(in JsonMemory left, in JsonMemory right) {
            return left.Equals(right);
        }

        public static bool operator !=(in JsonMemory left, in JsonMemory right) {
            return !(left == right);
        }

        public readonly override int GetHashCode() {
            return HashCode.Combine(this.Type, this.InnerValues, this.ReferenceData, this.KeyHash);
        }

        public static implicit operator JsonMemory(in JsonSpan span) {
            return Pin(in span);
        }
        public static implicit operator JsonSpan(in JsonMemory self) {
            if (self.KeyData.IsEmpty) {
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
                if (parsed.Type == JsonType.Null) {
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
                if (parsed.Type == JsonType.Null) {
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
                    if (data.KeyData.IsEmpty) {
                        return Empty;
                    }
                    else {
                        return new JsonMemory(data.KeyData.ToArray());
                    }
                case JsonType.Object:
                case JsonType.Array: // Inner bodies need re-parsing as the originals reference the same allocated memory and we want it to point to a new area
                    return ParseJson(data.KeyData.IsEmpty ? data.KeyData.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray().AsMemory());
                case JsonType.String:
                    return CreateString(data.KeyData.IsEmpty ? data.KeyData.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToString());
                case JsonType.Number:
                    return CreateNumber(data.KeyData.IsEmpty ? data.KeyData.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, double.Parse(data.GetValueAsSpan));
                case JsonType.Boolean:
                    return CreateBool(data.KeyData.IsEmpty ? data.KeyData.ToArray().AsMemory() : ReadOnlyMemory<char>.Empty, bool.Parse(data.GetValueAsSpan));
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

        private readonly ReadOnlySpan<ushort> source;
        private readonly int endIndex;
        public readonly int SourceLen;

        public int CurrentIndex { get; private set; }

        public readonly ushort CurrentValue => this.source[this.CurrentIndex];
        public readonly char CurrentChar => (char)this.CurrentValue;

        public readonly bool CanAdvance => this.CurrentIndex < this.endIndex;
        public readonly bool CanRetreat => this.CurrentIndex > 0;

        public JsonReader(in ReadOnlySpan<char> data, bool fromEnd = false) {
            this.source = MemoryMarshal.Cast<char, ushort>(data);
            this.SourceLen = data.Length;
            this.endIndex = this.SourceLen - 1;

            if (fromEnd) {
                this.CurrentIndex = data.Length;
            }
            else {
                this.CurrentIndex = -1;
            }
        }

        public override readonly string ToString() {
            return $"Reader: {{Source ..20: {(this.CurrentIndex < 0 || this.CurrentIndex > this.endIndex ? (this.CurrentIndex < 0 ? "[Read index at Start]" : "[Read index at End]") : MemoryMarshal.Cast<ushort, char>(this.source[Math.Max(0, this.CurrentIndex - 20)..this.CurrentIndex]).ToString())}, Pos: {this.CurrentIndex}, Char: '{(this.CurrentIndex < 0 || this.CurrentIndex > this.endIndex ? '\0' : this.CurrentIndex)}'}}";
        }

        public readonly ReadOnlySpan<ushort> Slice(int index, int len) {
            return this.source.Slice(index, len);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Increment() {
            if (this.CurrentIndex < this.endIndex) {
                this.CurrentIndex++;
                return true;
            }
            else {
                return false;
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Decrement() {
            if (this.CurrentIndex > 0) {
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
            this.CurrentIndex = this.SourceLen;
        }

        #region # ADVANCE #

        public ushort Advance(int amount = 1) {
            while (this.Increment() && --amount > 0) { }
            return this.CurrentValue;
        }

        public int AdvanceTo(ushort search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(search);
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus;
                    continue;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] > 0) {
                            this.CurrentIndex += i;
                            return this.CurrentIndex;
                        }
                    }
                    this.CurrentIndex += segmentMinus;
                    continue;
                }
            }
            return -1;
        }

        public int AdvanceToNot(ushort search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVectors = new Vector<ushort>(search);
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVectors);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentIndex;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] == 0) {
                            this.CurrentIndex += i;
                            return this.CurrentIndex;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return -1;
        }

        public ushort AdvanceTo(in ReadOnlySpan<ushort> search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            int searchLen = search.Length;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[searchLen];
            for (int i = 0; i < searchLen; i++) {
                searchVectors[i] = new Vector<ushort>(search[i]);
            }
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVectors[0]);
                for (int i = 1; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVectors[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] > 0) {
                            this.CurrentIndex += i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return '\0';
        }

        public ushort AdvanceToNot(in ReadOnlySpan<ushort> search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            int searchLen = search.Length;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[searchLen];
            for (int i = 0; i < searchLen; i++) {
                searchVectors[i] = new Vector<ushort>(search[i]);
            }
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVectors[0]);
                for (int i = 1; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVectors[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] == 0) {
                            this.CurrentIndex += i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return '\0';
        }

        public ushort AdvanceToWhiteSpace() {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(32); // 32 == ' '
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.GreaterThan(toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] == 0) {
                            this.CurrentIndex += i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return '\0';
        }

        public ushort AdvanceToNotWhiteSpace() {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(33); // 32 == ' '
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.LessThan(toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] == 0) {
                            this.CurrentIndex += i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return '\0';
        }

        public ushort AdvanceToCommaOrEndBrace() {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[] {
                new Vector<ushort>(COMMA),
                new Vector<ushort>(RBRACE),
                new Vector<ushort>(RBRACKET),
            };
            int searchLen = searchVectors.Length;
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVectors[0]);
                for (int i = 1; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVectors[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] > 0) {
                            this.CurrentIndex += i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return '\0';
        }

        public ushort AdvanceToValueEnding() {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[] {
                new Vector<ushort>(COMMA),
                new Vector<ushort>(RBRACE),
                new Vector<ushort>(RBRACKET),
            };
            Vector<ushort> whitespace = new Vector<ushort>(32);
            int searchLen = searchVectors.Length;
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Increment()) {
                if (this.CurrentIndex + segment >= this.SourceLen) {
                    buf.Clear();
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.LessThan(toCompare, whitespace);
                for (int i = 0; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVectors[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
                else {
                    for (int i = 0; i < segment; i++) {
                        if (eq[i] > 0) {
                            this.CurrentIndex += i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return '\0';
        }

        public int AdvanceToEndOfObject() {
            if (this.CurrentValue != LBRACE) {
                this.AdvanceTo(LBRACE);
            }
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[] {
                new Vector<ushort>(QUOTE),
                new Vector<ushort>(LBRACE),
                new Vector<ushort>(LBRACKET),
                new Vector<ushort>(RBRACE),
                new Vector<ushort>(RBRACKET),
            };
            int searchLen = searchVectors.Length;
            Span<ushort> buf = stackalloc ushort[segment];
            bool WithinQuotes = false;
            int debth = 1; // Already within the first brace
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            Continue:
            while (this.Increment()) {
                if (this.CurrentIndex + segment > this.endIndex) {
                    buf.Clear(); // Remove trailing chars
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVectors[0]);
                if (WithinQuotes) {
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    else {
                        for (int i = 0; i < segment; i++) {
                            if (eq[i] > 0) {
                                WithinQuotes = false;
                                this.CurrentIndex += i; // One Place after Quote
                                goto Continue;
                            }
                        }
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                }
                else {
                    for (int i = 1; i < searchLen; i++) {
                        eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVectors[i]));
                    }
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    else {
                        for (int i = 0; i < segment; i++) {
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
                        continue;
                    }
                }

            }
            return '\0';
        }

        public int AdvanceToEndOfArray() {
            if (this.CurrentValue != LBRACKET) {
                this.AdvanceTo(LBRACKET);
            }
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Span<Vector<ushort>> searchVectors = stackalloc Vector<ushort>[] {
                new Vector<ushort>(QUOTE),
                new Vector<ushort>(LBRACE),
                new Vector<ushort>(LBRACKET),
                new Vector<ushort>(RBRACE),
                new Vector<ushort>(RBRACKET),
            };
            int searchLen = searchVectors.Length;
            Span<ushort> buf = stackalloc ushort[segment];
            bool WithinQuotes = false;
            int debth = 1; // Already within the first brace
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            Continue:
            while (this.Increment()) {
                if (this.CurrentIndex + segment > this.endIndex) {
                    buf.Clear(); // Remove trailing chars
                    this.source.Slice(this.CurrentIndex).CopyTo(buf);
                }
                else {
                    this.source.Slice(this.CurrentIndex, segment).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVectors[0]);
                if (WithinQuotes) {
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    else {
                        for (int i = 0; i < segment; i++) {
                            if (eq[i] > 0) {
                                WithinQuotes = false;
                                this.CurrentIndex += i; // One Place after Quote
                                goto Continue;
                            }
                        }
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                }
                else {
                    for (int i = 1; i < searchLen; i++) {
                        eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVectors[i]));
                    }
                    if (eq == Vector<ushort>.Zero) {
                        this.CurrentIndex += segmentMinus; // Segments is one base so dont increment
                        continue;
                    }
                    else {
                        for (int i = 0; i < segment; i++) {
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
                        continue;
                    }
                }
            }
            return '\0';
        }

        #endregion


        #region # RETREAT #

        public ushort Retreat(int amount = 1) {
            while (this.Decrement() && --amount > 0) { }
            return this.CurrentValue;
        }

        public int RetreatTo(ushort search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(search);
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start > -1) {
                    this.source.Slice(start, segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
                else {
                    for (int i = segmentMinus; i >= 0; i--) {
                        if (eq[i] > 0) {
                            this.CurrentIndex = start + i;
                            return this.CurrentIndex;
                        }
                    }
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
            }
            return -1;
        }

        public int RetreatToNot(ushort search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(search);
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start > -1) {
                    this.source.Slice(start, segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentIndex;
                }
                else {
                    for (int i = segment - 1; i >= 0; i--) {
                        if (eq[i] == 0) {
                            this.CurrentIndex = start + i;
                            return this.CurrentIndex;
                        }
                    }
                    this.CurrentIndex -= segmentMinus; // Segments is one base so dont increment
                    continue;
                }
            }
            return -1;
        }

        public ushort RetreatTo(in ReadOnlySpan<ushort> search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            int searchLen = search.Length;
            Span<Vector<ushort>> searchVector = stackalloc Vector<ushort>[searchLen];
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            for (int i = 0; i < searchLen; i++) {
                searchVector[i] = new Vector<ushort>(search[i]);
            }
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start > -1) {
                    this.source.Slice(start, segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVector[0]);
                for (int i = 1; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVector[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
                else {
                    for (int i = segment - 1; i >= 0; i--) {
                        if (eq[i] > 0) {
                            this.CurrentIndex = start + i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
            }
            return '\0';
        }

        public ushort RetreatToNot(in ReadOnlySpan<ushort> search) {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            int searchLen = search.Length;
            Span<Vector<ushort>> searchVector = stackalloc Vector<ushort>[searchLen];
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            for (int i = 0; i < searchLen; i++) {
                searchVector[i] = new Vector<ushort>(search[i]);
            }
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start > -1) {
                    this.source.Slice(start, segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.Equals(toCompare, searchVector[0]);
                for (int i = 1; i < searchLen; i++) {
                    eq = Vector.BitwiseOr(eq, Vector.Equals(toCompare, searchVector[i]));
                }
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                else {
                    for (int i = segment - 1; i >= 0; i--) {
                        if (eq[i] == 0) {
                            this.CurrentIndex = start + i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
            }
            return '\0';
        }

        public ushort RetreatToWhiteSpace() {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(32); // 32 == ' ' 
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start > -1) {
                    this.source.Slice(start, segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.GreaterThan(toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                else {
                    for (int i = segment - 1; i >= 0; i--) {
                        if (eq[i] == 0) {
                            this.CurrentIndex = start + i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
            }
            return '\0';
        }

        public ushort RetreatToNotWhiteSpace() {
            int segment = Vector<ushort>.Count;
            int segmentMinus = segment - 1;
            Vector<ushort> searchVector = new Vector<ushort>(33); // 32 == ' ' 
            Span<ushort> buf = stackalloc ushort[segment];
            ref Vector<ushort> toCompare = ref MemoryMarshal.Cast<ushort, Vector<ushort>>(buf)[0];
            while (this.Decrement()) {
                int start = this.CurrentIndex - segmentMinus;
                if (start > -1) {
                    this.source.Slice(start, segment).CopyTo(buf);
                }
                else {
                    buf.Clear();
                    this.source.Slice(0, this.CurrentIndex + 1).CopyTo(buf);
                }
                Vector<ushort> eq = Vector.LessThan(toCompare, searchVector);
                if (eq == Vector<ushort>.Zero) {
                    return this.CurrentValue;
                }
                else {
                    for (int i = segment - 1; i >= 0; i--) {
                        if (eq[i] == 0) {
                            this.CurrentIndex = start + i;
                            return this.CurrentValue;
                        }
                    }
                    this.CurrentIndex -= segmentMinus;
                    continue;
                }
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

        /// <summary>
        /// Format used by the basic <c>.ToString()</c>
        /// </summary>
        public static ToStringFormat Default_ToStringFormat = ToStringFormat.Pretty | ToStringFormat.TranslateUnicode;

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
                return stackBuffer.Slice(0, x).ToString();
            }
            char[] buffer = ArrayPool<char>.Shared.Rent(len);
            Span<char> bufSpan = buffer.AsSpan(0, len);
            TranslateUnicodeIntoBufferFromSpan(in data, in bufSpan, ref x);
            string decodedValue = bufSpan.Slice(0, x).ToString();
            ArrayPool<char>.Shared.Return(buffer);
            return decodedValue;
        }

        public static int GetStringDecodeLengthFromSpan(in ReadOnlySpan<char> data) {
            int count = 0;
            int len = data.Length;
            int x = len;
            int check = Math.Max(len - 6, 0);
            while (x > check) {
                if (data[--x] == '\\') {
                    if (x + 5 < len && data[x + 1] == 'u') {
                        count -= 4; // -4 Hex numbers, +1 from 'u', '\' not counted
                    }
                }
                else {
                    count++;
                }
            }
            while (x > 0) {
                if (data[--x] == '\\') {
                    if (data[x + 1] == 'u') { // Now safe, len check removed
                        count -= 4; // -4 Hex numbers, +1 from 'u', '\' not counted
                    }
                }
                else {
                    count++;
                }
            }
            return count;
        }

        public static void TranslateUnicodeIntoBufferFromSpan(in ReadOnlySpan<char> data, in Span<char> buffer, ref int sbPos) {
            int len = data.Length;
            int x = -1;
            int y = 0;
            int requiredLen;
            while (++x < len) {
                if (data[x] == '\\') {
                    requiredLen = x - y;
                    data.Slice(y, requiredLen).CopyTo(buffer.Slice(sbPos, requiredLen));
                    sbPos += requiredLen;
                    if (++x < len) {
                        char current = data[x];
                        switch (current) {
                            case 'n':
                                buffer[sbPos++] = '\n';
                                break;
                            case 't':
                                buffer[sbPos++] = '\t';
                                break;
                            case 'u': {
                                if (x + 4 >= len) {
                                    break;
                                }
                                buffer[sbPos++] = (char)((ReadHexNumber(data[++x]) * 4096)
                                    + (ReadHexNumber(data[++x]) * 256)
                                    + (ReadHexNumber(data[++x]) * 16)
                                    + ReadHexNumber(data[++x]));
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
                    y = x + 1;
                }
            }
            requiredLen = len - y; // add data after final unicode to the end of the buffer
            data.Slice(y, requiredLen).CopyTo(buffer.Slice(sbPos, requiredLen));
            sbPos += requiredLen;
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
            return character < 33;
        }

        /// <summary>
        /// Returns true if char value is less than 33
        /// </summary>
        /// <param name="character"></param>
        /// <returns></returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsWhiteSpace(ushort character) {
            return character < 33;
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
            if (data[0] == '-' || data[0] == '+') {
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

        public static class JsonContainerPool {
            private const int knownMax = 1 << 20;
            private const int knownMin = 1 << 4;
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

            private static readonly Lazy<ArrayPool<JsonMemory>> ContainerPool = new Lazy<ArrayPool<JsonMemory>>(() => ArrayPool<JsonMemory>.Create(PoolMaxArrayLength, ArraysPerBucket), LazyThreadSafetyMode.PublicationOnly);

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal static JsonMemory[] Rent(int length) {
                if (length < 1 || length > PoolMaxArrayLength) {
                    throw new ArgumentOutOfRangeException(nameof(length), $"Length must be between 1 and {PoolMaxArrayLength}.");
                }
                return ContainerPool.Value.Rent(length);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal static void Return(JsonMemory[] array) {
                if (array == null) {
                    throw new ArgumentNullException(nameof(array));
                }
                ContainerPool.Value.Return(array, true);
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal static void EnsureBufferCapacity(int nextIndex, ref JsonMemory[] buffer) {
                if (nextIndex >= buffer.Length) {
                    JsonMemory[] newArray = ContainerPool.Value.Rent(nextIndex + 1);
                    buffer.CopyTo(newArray.AsSpan(0, buffer.Length));
                    ContainerPool.Value.Return(buffer, true); // Release memory references
                    buffer = newArray;
                }
            }

            internal sealed class RentedContainer : IDisposable, IEnumerable<JsonMemory> {
                public JsonMemory[] Value { get; private set; }

                public RentedContainer(int length) {
                    this.Value = ContainerPool.Value.Rent(length);
                }

#pragma warning disable CA1816 // Dispose methods should call SuppressFinalize
                public void Dispose() {
                    this.CleanUp(true);
                }

                ~RentedContainer() {
                    this.CleanUp(false);
                }

                private void CleanUp(bool suppress) {
                    lock (this.Value) {
                        if (this.Value != null) {
                            ContainerPool.Value.Return(this.Value, true);
                            this.Value = null;
                            if (suppress) {
                                GC.SuppressFinalize(this);
                            }
                        }
                    }
                }
#pragma warning restore CA1816 // Dispose methods should call SuppressFinalize

                public IEnumerator<JsonMemory> GetEnumerator() {
                    return ((IEnumerable<JsonMemory>)this.Value).GetEnumerator();
                }

                IEnumerator IEnumerable.GetEnumerator() {
                    return this.GetEnumerator();
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
        TranslateUnicode = 0x2,
        LowerCaseBool = 0x4,
        /// <summary>
        /// Numbers already pass the numerical test for validity, this option gets the number representation from <c>double</c> instead of the reference
        /// </summary>
        ReParseNumbers = 0x8,

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
    }

    #endregion
}
