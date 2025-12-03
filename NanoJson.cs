///////////////////////////////////////////////////
///												///
///		NanoJson by Duncan 'ScoredOne' Mellor	///
///												///
///		Released under the MIT license			///
///												///
///		Thursday 27th November 2025				///
///												///
///		Software provided in as-is condition	///
///												///
///////////////////////////////////////////////////

using System;
using System.Buffers;
using System.Collections;
using System.Collections.Generic;

namespace NanoJson {

	public readonly struct NanoJson : IEnumerable<NanoJson>, IEquatable<NanoJson> {

		private const string INDENT_TABS = "   "; // Can be better, indenting seems off
		private const int INDENT_LEN = 3;

		public static class WordBank {
			public const string TRUE = "true";
			public const string FALSE = "false";
			public const string NULL = "null";
		}

		public static NanoJson ParseJson(string key, string data) {
			if (string.IsNullOrWhiteSpace(key)) {
				return NanoJson.ParseJson(data);
			}
			else {
				return new NanoJson(key.AsMemory(), data.AsMemory(), false, -1);
			}
		}

		public static NanoJson ParseJson(string data) {
			return new NanoJson(data.AsMemory(), false, -1);
		}

		public static NanoJson CreateArray(string key, NanoJson[] data) {
			return new NanoJson(key, JsonType.Array, data);
		}

		public static NanoJson CreateArray(NanoJson[] data) {
			return new NanoJson(JsonType.Array, data);
		}

		public static NanoJson CreateObject(string key, NanoJson[] data) {
			return new NanoJson(key, JsonType.Object, data);
		}

		public static NanoJson CreateObject(NanoJson[] data) {
			return new NanoJson(JsonType.Object, data);
		}

		public static NanoJson CreateStringObject(string key, string data) {
			return new NanoJson(key.AsMemory(), data.AsMemory(), true, -1);
		}

		public static NanoJson CreateStringObject(string data) {
			return new NanoJson(data.AsMemory(), false, -1);
		}

		public static NanoJson ContainValueInObject(string key, NanoJson data) {
			return new NanoJson(key, data);
		}

		public static NanoJson CreateBoolObject(string key, bool data) {
			return new NanoJson(key, data);
		}

		public static NanoJson CreateNumberObject(string key, double data) {
			return new NanoJson(key, data);
		}

		private readonly static NanoJson empty = new NanoJson();
		public static NanoJson Empty => empty;

		public enum JsonType {
			/// <summary>
			/// <c>null</c>
			/// </summary>
			Null,
			/// <summary>
			/// <c>NanoJson[]</c>
			/// </summary>
			Object,
			/// <summary>
			/// <c>NanoJson[]</c>
			/// </summary>
			Array,
			/// <summary>
			/// <c>string</c>
			/// </summary>
			String,
			/// <summary>
			/// <c>bool</c>
			/// </summary>
			Boolean,
			/// <summary>
			/// <c>double</c>
			/// </summary>
			Number
		}

		public readonly JsonType Type;
		private readonly NanoJson[] InnerValues;
		private readonly ReadOnlyMemory<char> ReferenceData;
		private readonly ReadOnlyMemory<char> KeyData;
		private readonly int InnerCount;

		private NanoJson(ReadOnlyMemory<char> key, ReadOnlyMemory<char> data, bool literal = false, int innerLength = -1, int knownLen = -1) : this(data, literal, innerLength, knownLen) {
			this.KeyData = key;
		}

		private NanoJson(ReadOnlyMemory<char> reference, bool literal = false, int innerLength = -1, int knownLen = -1) {
			this.KeyData = ReadOnlyMemory<char>.Empty;
			this.InnerCount = innerLength;
			this.Type = JsonType.Null;
			this.InnerValues = Array.Empty<NanoJson>();
			this.ReferenceData = reference;
			if (literal) {
				this.Type = JsonType.String;
			}
			else {
				if (reference.IsEmpty) {
					return;
				}
				ReadOnlySpan<char> data = reference.Span;
				int len = knownLen == -1 ? data.Length : knownLen;
				int x = 0;
				int first;
				int y;
				int debth;
				int items;
				char c;
				while (char.IsWhiteSpace(c = data[x])) { x++; }
				switch (data[x]) {
					case '"':
						this.Type = JsonType.String;
						first = ++x;
						x = len;
						while (--x > first) {
							if (data[x] == '"') {
								if (first > x) {
									throw new ArgumentException("Parse failed", nameof(reference));
								}
								else if (first == x) {
									this.ReferenceData = string.Empty.AsMemory();
									return;
								}
								else {
									this.ReferenceData = reference[first..x];
									return;
								}
							}
						}
						throw new ArgumentException("Parse failed", nameof(reference));
					case '[':
						this.Type = JsonType.Array;
						y = len - 1;
						first = x;

						while (y > x) {
							if (data[y] == ']') {
								break;
							}
							y--;
						}

						if (y <= x) {
							throw new ArgumentException("Parse failed", nameof(reference));
						}
						while (char.IsWhiteSpace(data[++x])) { }
						if (x == y) {
							this.InnerCount = 0;
							return;
						}
						y++;
						x = first;
						debth = 0;

						if (this.InnerCount == -1) {
							items = 1;
							while (x < len) {
								switch (data[x]) {
									case '"':
										while (data[++x] != '"') { }
										break;
									case '{':
									case '[':
										debth++;
										break;
									case '}':
									case ']':
										debth--;
										break;
									case ',':
										if (debth == 1) {
											items++;
										}
										break;
								}
								x++;
							}
							this.InnerCount = items; // Cant set this value from methods
						}
						this.InnerValues = new NanoJson[this.InnerCount];

						this.ProcessJsonArray(reference[first..y], y - first);
						return;
					case '{':
						this.Type = JsonType.Object;
						first = x;
						y = len - 1;

						while (y > x) {
							if (data[y] == '}') {
								break;
							}
							y--;
						}

						if (y <= x) {
							throw new ArgumentException("Parse failed", nameof(reference));
						}
						while (char.IsWhiteSpace(data[++x])) { }
						if (x == y) {
							this.InnerCount = 0;
							return;
						}
						y++;
						x = first;
						debth = 0;
						if (this.InnerCount == -1) {
							items = 1;
							while (x < len) {
								switch (data[x]) {
									case '"':
										while (data[++x] != '"') { }
										break;
									case '{':
									case '[':
										debth++;
										break;
									case '}':
									case ']':
										debth--;
										break;
									case ',':
										if (debth == 1) {
											items++;
										}
										break;
								}
								x++;
							}
							this.InnerCount = items; // Cant set this value from methods
						}
						this.InnerValues = new NanoJson[this.InnerCount];

						this.ProcessJsonObject(reference[first..y], y - first);
						return;
					case 't':
					case 'T':
						this.Type = JsonType.Boolean;
						if (len - x > 3) {
							c = data[++x];
							if (c == 'r' || c == 'R') {
								c = data[++x];
								if (c == 'u' || c == 'U') {
									c = data[++x];
									if (c == 'e' || c == 'E') {
										if (++x < len && !data[x..].IsWhiteSpace()) {
											throw new ArgumentException("Parse failed", nameof(reference));
										}
										this.ReferenceData = WordBank.TRUE.AsMemory();
										return;
									}
								}
							}
						}

						throw new ArgumentException("Parse failed", nameof(reference));
					case 'f':
					case 'F':
						this.Type = JsonType.Boolean;
						if (len - x > 4) {
							c = data[++x];
							if (c == 'a' || c == 'A') {
								c = data[++x];
								if (c == 'l' || c == 'L') {
									c = data[++x];
									if (c == 's' || c == 'S') {
										c = data[++x];
										if (c == 'e' || c == 'E') {
											if (++x < len && !data[x..].IsWhiteSpace()) {
												throw new ArgumentException("Parse failed", nameof(reference));
											}
											this.ReferenceData = WordBank.FALSE.AsMemory();
											return;
										}
									}
								}
							}
						}

						throw new ArgumentException("Parse failed", nameof(reference));
					case 'n':
					case 'N':
						this.Type = JsonType.Null;
						if (len - x > 3) {
							c = data[++x];
							if (c == 'u' || c == 'U') {
								c = data[++x];
								if (c == 'l' || c == 'L') {
									c = data[++x];
									if (c == 'l' || c == 'L') {
										if (++x < len && !data[x..].IsWhiteSpace()) {
											throw new ArgumentException("Parse failed", nameof(reference));
										}
										this.ReferenceData = WordBank.NULL.AsMemory();
										return;
									}
								}
							}
						}

						throw new ArgumentException("Parse failed", nameof(reference));
					case '-':
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
						first = x;
						if (x > 0 && !char.IsWhiteSpace(data[x - 1])) {
							throw new ArgumentException("Parse failed", nameof(reference));
						}
						this.Type = JsonType.Number;

						if (data[x] == '-') {
							x++;
						}
						bool dec = false;
						bool E = false;

						while (x < len) {
							c = data[x];
							switch (c) {
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
									x++;
									continue;
								case '.':
									if (dec
										|| ++x == len
										|| !char.IsDigit(data[x])) {
										throw new ArgumentException("Parse failed", nameof(reference));
									}
									dec = true;
									x++;
									continue;
								case 'e':
								case 'E':
									if (E
										|| ++x == len
										|| ((c = data[x]) != '+' && c != '-')
										|| ++x == len
										|| !char.IsDigit(data[++x])) {
										throw new ArgumentException("Parse failed", nameof(reference));
									}
									E = true;
									x++;
									continue;
								default:
									if (char.IsWhiteSpace(c)) {
										x++;
										continue;
									}
									throw new ArgumentException("Parse failed", nameof(reference));
							}
						}
						this.ReferenceData = reference[first..x];

						return;
				}
			}
		}

		private NanoJson(JsonType type, params NanoJson[] contents) : this(ReadOnlyMemory<char>.Empty, type, contents) { }

		private NanoJson(string key, JsonType type, params NanoJson[] contents) : this(key.AsMemory(), type, contents) { }

		private NanoJson(ReadOnlyMemory<char> key, JsonType type, params NanoJson[] contents) {
			switch (type) {
				case JsonType.Array:
				case JsonType.Object:
					this.Type = type;
					this.InnerValues = contents;
					this.KeyData = key;
					this.ReferenceData = ReadOnlyMemory<char>.Empty;
					this.InnerCount = contents.Length;
					break;
				default:
					throw new NotSupportedException();
			}
		}

		private NanoJson(string key, NanoJson value) : this(key.AsMemory(), value) { }
		private NanoJson(ReadOnlyMemory<char> key, NanoJson value) {
			this.KeyData = key;
			this.Type = value.Type;
			this.ReferenceData = value.ReferenceData;
			this.InnerValues = value.InnerValues;
			this.InnerCount = value.InnerCount;
		}

		private NanoJson(string key, bool value) : this(key.AsMemory(), value) { }
		private NanoJson(ReadOnlyMemory<char> key, bool value) {
			this.KeyData = key;
			this.Type = JsonType.Boolean;
			this.ReferenceData = value ? WordBank.TRUE.AsMemory() : WordBank.FALSE.AsMemory();
			this.InnerValues = Array.Empty<NanoJson>();
			this.InnerCount = -1;
		}

		private NanoJson(string key, double value) : this(key.AsMemory(), value) { }
		private NanoJson(ReadOnlyMemory<char> key, double value) {
			this.KeyData = key;
			this.Type = JsonType.Boolean;
			this.ReferenceData = value.ToString().AsMemory();
			this.InnerValues = Array.Empty<NanoJson>();
			this.InnerCount = -1;
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key">Key or Path to desired value</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException">Key was not found in object</exception>
		/// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
		public NanoJson this[string path] => this[path.AsSpan()];

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key">Key or Path to desired value</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException">Key was not found in object</exception>
		/// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
		public NanoJson this[ReadOnlySpan<char> key]
		{
			get
			{
				int pathLen = key.Length;
				switch (this.Type) {
					case JsonType.Object:
						if (this.ContainsKey(key, out NanoJson found)) {
							return found;
						}
						throw new ArgumentException($"Path provided was invalid [{key.ToString()}]", nameof(key));
					default:
						throw new InvalidOperationException();
				}
			}
		}

		/// <summary>
		/// Get value at index of the contained NanoJson values
		/// </summary>
		/// <param name="index"></param>
		/// <returns></returns>
		/// <exception cref="IndexOutOfRangeException"></exception>
		public NanoJson this[int index]
		{
			get
			{
				switch (this.Type) {
					case JsonType.Object:
					case JsonType.Array:
						return this.InnerValues[index];
					default:
						throw new IndexOutOfRangeException();
				}
			}
		}

		/// <summary>
		/// Process known JsonArray to populated this.InnerValues
		/// </summary>
		/// <param name="reference">Value found after the colon</param>
		/// <param name="len">Length of the reference area</param>
		private void ProcessJsonArray(ReadOnlyMemory<char> reference, int len) {
			ReadOnlySpan<char> data = reference.Span;
			int x = 0;
			while (data[x++] != '[') { }
			int y = x;
			int debth = 1;
			int index = 0;
			int innerSize;

			while (true) {
				innerSize = 1;
				while (true) {
					switch (data[x]) {
						case '"':
							while (data[++x] != '"') { }
							break;
						case '[':
						case '{':
							debth++;
							break;
						case '}':
							debth--;
							break;
						case ',':
							if (debth == 1) {
								goto ProcessJsonObject;
							}
							else if (debth == 2) {
								innerSize++;
							}
							break;
						case ']':
							if (--debth == 0) {
								goto ProcessJsonObject;
							}
							break;
					}
					x++;
				}

				ProcessJsonObject:
				this.InnerValues[index++] = new NanoJson(reference[y..x], false, innerSize, x - y);
				if (index == this.InnerCount) {
					return;
				}
				y = ++x;
			}
		}

		/// <summary>
		/// Process known JsonObject to populated this.InnerValues
		/// </summary>
		/// <param name="reference">Value found after the colon</param>
		/// <param name="len">Length of the reference area</param>
		private void ProcessJsonObject(ReadOnlyMemory<char> reference, int len) {
			ReadOnlySpan<char> data = reference.Span;

			int x = 0;
			int y = 0;
			int debth = 0;
			int index = 0;
			int innerSize;

			while (true) {
				if (data[x] != '"') {
					x++;
					continue;
				}
				ReadOnlyMemory<char> name;
				if (data[++x] == '"') {
					name = ReadOnlyMemory<char>.Empty;
				}
				else {
					y = x;
					while (data[++x] != '"') { }
					name = reference[y..x];
				}

				while (data[++x] != ':') { }
				while (char.IsWhiteSpace(data[++x])) { }
				y = x;
				innerSize = 1;
				while (true) {
					switch (data[x]) {
						case '"':
							while (data[++x] != '"') { }
							break;
						case '{':
						case '[':
							debth++;
							break;
						case '}':
						case ']':
							if (--debth < 0) { // no comma found, process last segment
								goto ProcessJsonObject;
							}
							break;
						case ',':
							if (debth == 0) {
								goto ProcessJsonObject;
							}
							else if (debth == 1) {
								innerSize++;
							}
							break;
					}
					x++;
				}

				ProcessJsonObject:
				this.InnerValues[index++] = new NanoJson(name, reference[y..x], false, innerSize, x - y);
				if (index == this.InnerCount) {
					return;
				}
			}
		}

		public override string ToString() => this.ToString(false);

		public string ToString(bool pretty) {
			int count = 0;
			this.CalculateStringSize(pretty, ref count);

			char[] buffer = ArrayPool<char>.Shared.Rent(count);
			Memory<char> bufferMemory = buffer.AsMemory()[..count];
			this.ProcessString(false, pretty, in bufferMemory);

			string builtString = new string(bufferMemory.Span);
			ArrayPool<char>.Shared.Return(buffer);
			return builtString;
		}

		private void CalculateStringSize(bool pretty, ref int count) {
			int indent = 0;
			this.CalculateStringSize(false, pretty, ref count, ref indent);
		}

		private void CalculateStringSize(bool AsValue, bool pretty, ref int count, ref int indent) {
			switch (this.Type) {
				case JsonType.String:
					count += this.ReferenceData.Length + 2;
					break;
				case JsonType.Null:
				case JsonType.Number:
				case JsonType.Boolean:
					count += this.ReferenceData.Length;
					break;
				case JsonType.Object:
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							count += INDENT_LEN;
						}
					}
					count++;

					if (this.InnerCount == 0) {
						if (pretty) {
							count += INDENT_LEN;
						}
						count++;
						break;
					}
					indent++;
					if (pretty) {
						count++;
					}
					for (int x = 0; x < this.InnerCount; x++) {
						NanoJson value = this.InnerValues[x];

						if (pretty) {
							for (int y = 0; y < indent; y++) {
								count += INDENT_LEN;
							}
							count += value.KeyData.Length + 4;
							value.CalculateStringSize(true, pretty, ref count, ref indent);
							if (x < this.InnerCount - 1) {
								count++;
							}
							count++;
						}
						else {
							count += value.KeyData.Length + 4;
							value.CalculateStringSize(true, pretty, ref count, ref indent);
							if (x < this.InnerCount - 1) {
								count++;
							}
						}
					}

					indent--;
					if (pretty) {
						for (int x = 0; x < indent; x++) {
							count += INDENT_LEN;
						}
					}
					count++;
					break;
				case JsonType.Array:
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							count += INDENT_LEN;
						}
					}
					count++;

					if (this.InnerCount == 0) {
						if (pretty) {
							count += INDENT_LEN;
						}
						count++;
					}
					else {
						indent++;
						if (pretty) {
							count++;
						}
						for (int x = 0; x < this.InnerCount; x++) {
							NanoJson value = this.InnerValues[x];
							if (pretty) {
								switch (value.Type) {
									case JsonType.Null:
									case JsonType.String:
									case JsonType.Number:
									case JsonType.Boolean:
										for (int y = 0; y < indent; y++) {
											count += INDENT_LEN;
										}
										break;
								}
								value.CalculateStringSize(false, pretty, ref count, ref indent);
								if (x < this.InnerCount - 1) {
									count++;
								}
								count++;
							}
							else {
								value.CalculateStringSize(false, pretty, ref count, ref indent);
								if (x < this.InnerCount - 1) {
									count++;
								}
							}
						}

						indent--;
						if (pretty) {
							for (int x = 0; x < indent; x++) {
								count += INDENT_LEN;
							}
						}
						count++;
					}
					break;
			}
		}

		/// <summary>
		/// Recursive method to build the json ToString output
		/// </summary>
		/// <param name="AsValue"></param>
		/// <param name="pretty"></param>
		/// <param name="sb"></param>
		private void ProcessString(bool AsValue, bool pretty, in Memory<char> sb) {
			int indent = 0;
			int pos = 0;
			this.ProcessString(AsValue, pretty, ref indent, in sb, ref pos);
		}
		/// <summary>
		/// Recursive method to build the json ToString output
		/// </summary>
		/// <param name="AsValue"></param>
		/// <param name="pretty"></param>
		/// <param name="indent"></param>
		/// <param name="sb"></param>
		private void ProcessString(bool AsValue, bool pretty, ref int indent, in Memory<char> sb, ref int sbPos) {
			Span<char> data = sb.Span;
			ReadOnlySpan<char> indentSpan;
			switch (this.Type) {
				case JsonType.String:
					data[sbPos++] = '"';
					this.ReferenceData.Span.CopyTo(data[sbPos..(sbPos += this.ReferenceData.Length)]);
					data[sbPos++] = '"';
					break;
				case JsonType.Null:
				case JsonType.Number:
				case JsonType.Boolean:
					this.ReferenceData.Span.CopyTo(data[sbPos..(sbPos += this.ReferenceData.Length)]);
					break;
				case JsonType.Object:
					indentSpan = pretty ? INDENT_TABS.AsSpan() : ReadOnlySpan<char>.Empty;
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
						}
					}
					data[sbPos++] = '{';

					if (this.InnerCount == 0) {
						if (pretty) {
							indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
						}
						data[sbPos++] = '}';
						break;
					}
					indent++;
					if (pretty) {
						data[sbPos++] = '\n';
					}
					for (int x = 0; x < this.InnerCount; x++) {
						NanoJson value = this.InnerValues[x];

						if (pretty) {
							for (int y = 0; y < indent; y++) {
								indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
							}
							data[sbPos++] = '"';
							value.KeyData.Span.CopyTo(data[sbPos..(sbPos += value.KeyData.Length)]);
							data[sbPos++] = '"';
							data[sbPos++] = ':';
							data[sbPos++] = ' ';
							value.ProcessString(true, pretty, ref indent, in sb, ref sbPos);
							if (x < this.InnerCount - 1) {
								data[sbPos++] = ',';
							}
							data[sbPos++] = '\n';
						}
						else {
							data[sbPos++] = '"';
							value.KeyData.Span.CopyTo(data[sbPos..(sbPos += value.KeyData.Length)]);
							data[sbPos++] = '"';
							data[sbPos++] = ':';
							data[sbPos++] = ' ';
							value.ProcessString(true, pretty, ref indent, in sb, ref sbPos);
							if (x < this.InnerCount - 1) {
								data[sbPos++] = ',';
							}
						}
					}

					indent--;
					if (pretty) {
						for (int x = 0; x < indent; x++) {
							indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
						}
					}
					data[sbPos++] = '}';
					break;
				case JsonType.Array:
					indentSpan = pretty ? INDENT_TABS.AsSpan() : ReadOnlySpan<char>.Empty;
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
						}
					}
					data[sbPos++] = '[';

					if (this.InnerCount == 0) {
						if (pretty) {
							indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
						}
						data[sbPos++] = ']';
					}
					else {
						indent++;
						if (pretty) {
							data[sbPos++] = '\n';
						}
						for (int x = 0; x < this.InnerCount; x++) {
							NanoJson value = this.InnerValues[x];
							if (pretty) {
								switch (value.Type) {
									case JsonType.Null:
									case JsonType.String:
									case JsonType.Number:
									case JsonType.Boolean:
										for (int y = 0; y < indent; y++) {
											indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
										}
										break;
								}
								value.ProcessString(false, pretty, ref indent, in sb, ref sbPos);
								if (x < this.InnerCount - 1) {
									data[sbPos++] = ',';
								}
								data[sbPos++] = '\n';
							}
							else {
								value.ProcessString(false, pretty, ref indent, in sb, ref sbPos);
								if (x < this.InnerCount - 1) {
									data[sbPos++] = ',';
								}
							}
						}

						indent--;
						if (pretty) {
							for (int x = 0; x < indent; x++) {
								indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
							}
						}
						data[sbPos++] = ']';
					}
					break;
			}
		}

		public int EstablishNumContainersRequired(ReadOnlySpan<char> data, int len) {
			int x = 0;
			int containers = 0;
			char c;
			int y;
			while (x < len) {
				c = data[x];
				switch (c) {
					case '"':
						while (data[++x] != '"') { }
						break;
					case '{':
					case '[':
						containers++;
						break;
					case '}':
						y = x;
						while (char.IsWhiteSpace(c = data[--y])) { }
						if (c == '{') {
							containers--;
						}
						break;
					case ']':
						y = x;
						while (char.IsWhiteSpace(c = data[--y])) { }
						if (c == '[') {
							containers--;
						}
						break;
					case ',':
						containers++;
						break;
				}
				x++;
			}
			return containers;
		}

		public string GetString => this.ReferenceData.ToString();

		/// <summary>
		/// Get the data used inside This object
		/// </summary>
		public ReadOnlySpan<char> GetSpan => this.ReferenceData.Span;

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public double GetNumber => double.Parse(this.ReferenceData.Span);


		/// <summary>
		/// Get the values contained inside This object but as a new array
		/// </summary>
		public NanoJson[] GetCopyOfContainedArray
		{
			get
			{
				NanoJson[] copy = new NanoJson[this.InnerCount];
				this.InnerValues.CopyTo(copy, 0);
				return copy;
			}
		}

		/// <summary>
		/// Get if This object is Null
		/// </summary>
		public bool IsNull => this.Type == JsonType.Null;

		/// <summary>
		/// Get the bool value of This object
		/// </summary>
		public bool GetBool => bool.Parse(this.ReferenceData.Span);

		/// <summary>
		/// Get the key of This object
		/// </summary>
		public string GetKey
		{
			get
			{
				if (this.KeyData.IsEmpty) {
					return string.Empty;
				}
				else {
					return this.KeyData.ToString();
				}
			}
		}

		/// <summary>
		/// Get the key of This object
		/// </summary>
		public ReadOnlySpan<char> GetKeyAsSpan
		{
			get
			{
				if (this.KeyData.IsEmpty) {
					return ReadOnlySpan<char>.Empty;
				}
				else {
					return this.KeyData.Span;
				}
			}
		}

		/// <summary>
		/// Compare the key of This object
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool CompareKey(string key) => this.CompareKey(key.AsSpan());
		/// <summary>
		/// Compare the key of This object
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool CompareKey(ReadOnlySpan<char> key) {
			return MemoryExtensions.SequenceEqual(key, KeyData.Span);
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key Found</returns>
		public bool ContainsKey(string key, out NanoJson found) => this.ContainsKey(key.AsSpan(), out found);
		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key Found</returns>
		public bool ContainsKey(ReadOnlySpan<char> key, out NanoJson found) {
			int pathLen = key.Length;
			for (int x = 0; x < this.InnerCount; x++) {
				NanoJson value = this.InnerValues[x];
				ReadOnlySpan<char> valueKey = value.KeyData.Span;
				int len = valueKey.Length;
				if (pathLen == len) {
					if (key.StartsWith(valueKey)) {
						found = value;
						return true;
					}
				}
				else if (pathLen > len) {
					if (key.StartsWith(valueKey) && value.ContainsKey(key[++len..], out found)) {
						return true;
					}
				}
			}

			found = NanoJson.Empty;
			return false;
		}

		public IEnumerator<NanoJson> GetEnumerator() {
			switch (this.Type) {
				case JsonType.Object:
				case JsonType.Array:
					foreach (NanoJson v in this.InnerValues) {
						yield return v;
					}
					break;
				default:
					yield return this;
					break;
			}
		}

		IEnumerator IEnumerable.GetEnumerator() => this.GetEnumerator();

		public override bool Equals(object obj) => base.Equals((NanoJson)obj);
		public bool Equals(NanoJson other) {
			if (this.Type.Equals(other.Type)
				&& this.InnerCount.Equals(other.InnerCount)
				&& this.CompareKey(other.KeyData.Span)
				&& MemoryExtensions.Equals(this.ReferenceData, other.ReferenceData)) {
				return true;
			}
			return false;
		}
		public static bool operator ==(NanoJson left, NanoJson right) {
			return left.Equals(right);
		}

		public static bool operator !=(NanoJson left, NanoJson right) {
			return !(left == right);
		}

		public override int GetHashCode() {
			unchecked {
				int hash = (int)2166136261;
				hash = (hash * 16777619) ^ this.Type.GetHashCode();
				hash = (hash * 16777619) ^ this.InnerValues.GetHashCode();
				hash = (hash * 16777619) ^ this.ReferenceData.GetHashCode();
				hash = (hash * 16777619) ^ this.KeyData.GetHashCode();
				hash = (hash * 16777619) ^ this.InnerCount.GetHashCode();
				return hash;
			}
		}
	}
}
