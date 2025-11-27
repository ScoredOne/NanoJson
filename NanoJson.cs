using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace NanoJson {

	public readonly struct NanoJson : IEnumerable<NanoJson> {

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
			this.InnerCount = -1;
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
				while (x < len) {
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
							this.InnerCount = innerLength;
							this.Type = JsonType.Array;
							y = len - 1;
							first = x;

							while (x < len) {
								if (data[x] == '[') {
									break;
								}
								x++;
							}
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
								this.InnerValues = Array.Empty<NanoJson>();
								this.InnerCount = 0;
								return;
							}

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

							this.ProcessJsonArray(reference[first..], len - first);
							return;
						case '{':
							this.InnerCount = innerLength;
							this.Type = JsonType.Object;
							first = x;
							y = len - 1;

							while (x < len) {
								if (data[x] == '{') {
									break;
								}
								x++;
							}
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
								this.InnerValues = Array.Empty<NanoJson>();
								this.InnerCount = 0;
								return;
							}

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

							this.ProcessJsonObject(reference[first..], len - first);
							return;
						case 't':
						case 'T':
							this.Type = JsonType.Boolean;
							if (len - x > 3) {
								char c = data[++x];
								if (c == 'r' || c == 'R') {
									c = data[++x];
									if (c == 'u' || c == 'U') {
										c = data[++x];
										if (c == 'e' || c == 'E') {
											if (++x < len && data[x..].IsWhiteSpace()) {
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
								char c = data[++x];
								if (c == 'a' || c == 'A') {
									c = data[++x];
									if (c == 'l' || c == 'L') {
										c = data[++x];
										if (c == 's' || c == 'S') {
											c = data[++x];
											if (c == 'e' || c == 'E') {
												if (++x < len && data[x..].IsWhiteSpace()) {
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
								char c = data[++x];
								if (c == 'u' || c == 'U') {
									c = data[++x];
									if (c == 'l' || c == 'L') {
										c = data[++x];
										if (c == 'l' || c == 'L') {
											if (++x < len && data[x..].IsWhiteSpace()) {
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
								char c = data[x];
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
					x++;
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
			this.InnerCount = -1;
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

		public NanoJson this[string path] => this[path.AsSpan()];

		public NanoJson this[ReadOnlySpan<char> path]
		{
			get
			{
				int pathLen = path.Length;
				switch (this.Type) {
					case JsonType.Object:
						for (int x = 0; x < this.InnerCount; x++) {
							NanoJson value = this.InnerValues[x];
							ReadOnlySpan<char> valueKey = value.KeyData.Span;
							int len = valueKey.Length;
							if (pathLen >= len && path.StartsWith(valueKey)) {
								if (pathLen == len) {
									return value;
								}
								else if (path[len] == '.') {
									return value[path[++len..]];
								}
							}
						}
						throw new ArgumentException($"Path provided was invalid [{path.ToString()}]", nameof(path));
					default:
						throw new InvalidOperationException();
				}
			}
		}

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

		private void ProcessJsonArray(ReadOnlyMemory<char> reference, int len) {
			ReadOnlySpan<char> data = reference.Span;
			int x = 0;
			int y = 0;
			int debth = 0;

			NanoJson[] container = this.InnerValues;

			int index = 0;
			int innerSize;

			while (x < len) {
				innerSize = 1;
				while (x < len) {
					switch (data[x]) {
						case '"':
							while (data[++x] != '"') { }
							break;
						case '[':
							if (debth++ == 0) {
								y = ++x;
								continue;
							}
							break;
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
				container[index++] = new NanoJson(reference[y..x], false, innerSize, x - y);
				y = x++;
			}
		}

		private void ProcessJsonObject(ReadOnlyMemory<char> reference, int len) {
			ReadOnlySpan<char> data = reference.Span;

			int x = 0;
			int y = len - 1;
			int debth = 0;
			NanoJson[] container = this.InnerValues;

			int index = 0;
			int innerSize;

			while (x < len) {
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
				container[index++] = new NanoJson(name, reference[y..x], false, innerSize, x - y);
			}
		}

		public override string ToString() => this.ToString(false);

		public string ToString(bool pretty) {
			int indent = 0;
			StringBuilder sb = new StringBuilder();
			this.ProcessString(false, pretty, ref indent, in sb);
			return sb.ToString();
		}

		private void ProcessString(bool AsValue, bool pretty, ref int indent, in StringBuilder sb) { // Can be better, indenting seems off
			const string tabs = "   ";

			int refLen;
			switch (this.Type) {
				case JsonType.String:
					refLen = this.ReferenceData.Length;
					Span<char> appendString = stackalloc char[refLen + 2];
					appendString[0] = '"';
					this.ReferenceData.Span.CopyTo(appendString[1..]);
					appendString[refLen + 1] = '"';
					sb.Append(appendString);
					break;
				case JsonType.Null:
				case JsonType.Number:
				case JsonType.Boolean:
					sb.Append(this.ReferenceData);
					break;
				case JsonType.Object:
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							sb.Append(tabs);
						}
					}
					sb.Append('{');
					indent++;

					if (this.InnerCount == 0) {
						indent--;
						if (pretty) {
							sb.Append(tabs);
						}
						sb.Append('}');
						break;
					}
					if (pretty) {
						sb.Append('\n');
					}
					for (int x = 0; x < this.InnerCount; x++) {
						NanoJson value = this.InnerValues[x];

						if (pretty) {
							for (int y = 0; y < indent; y++) {
								sb.Append(tabs);
							}
							sb.Append('\"');
							sb.Append(value.KeyData);
							sb.Append('\"');
							sb.Append(':');
							sb.Append(' ');
							value.ProcessString(true, pretty, ref indent, in sb);
							if (x < this.InnerCount - 1) {
								sb.Append(',');
							}
							sb.Append('\n');
						}
						else {
							sb.Append('\"');
							sb.Append(value.KeyData);
							sb.Append('\"');
							sb.Append(':');
							sb.Append(' ');
							value.ProcessString(true, pretty, ref indent, in sb);
							if (x < this.InnerCount - 1) {
								sb.Append(',');
							}
						}
					}

					indent--;
					if (pretty) {
						for (int x = 0; x < indent; x++) {
							sb.Append(tabs);
						}
					}
					sb.Append('}');
					break;
				case JsonType.Array:
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							sb.Append(tabs);
						}
					}
					sb.Append('[');
					indent++;

					if (this.InnerCount == 0) {
						indent--;
						if (pretty) {
							sb.Append(tabs);
						}
						sb.Append(']');
					}
					else {
						if (pretty) {
							sb.Append('\n');
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
											sb.Append(tabs);
										}
										break;
								}
								value.ProcessString(false, pretty, ref indent, in sb);
								if (x < this.InnerCount - 1) {
									sb.Append(',');
								}
								sb.Append('\n');
							}
							else {
								value.ProcessString(false, pretty, ref indent, in sb);
								if (x < this.InnerCount - 1) {
									sb.Append(',');
								}
							}
						}

						indent--;
						if (pretty) {
							for (int x = 0; x < indent; x++) {
								sb.Append(tabs);
							}
						}
						sb.Append(']');
					}
					break;
			}
		}

		public string GetString => this.ReferenceData.ToString();

		public ReadOnlySpan<char> GetSpan => this.ReferenceData.Span;

		public double GetValue => double.Parse(this.ReferenceData.Span);

		public NanoJson[] GetContainedArray => this.InnerValues;

		public bool IsNull => this.Type == JsonType.Null;

		public bool GetBool => bool.Parse(this.ReferenceData.Span);

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

		public bool CompareKey(string key) => this.CompareKey(key.AsSpan());
		public bool CompareKey(ReadOnlyMemory<char> key) => this.CompareKey(key.Span);
		public bool CompareKey(ReadOnlySpan<char> key) {
			return MemoryExtensions.SequenceEqual(key, KeyData.Span);
		}

		public bool ContainsKey(string key) => this.ContainsKey(key.AsSpan());
		public bool ContainsKey(ReadOnlyMemory<char> key) => this.ContainsKey(key.Span);
		public bool ContainsKey(ReadOnlySpan<char> key) {
			int indexSeperator = key.IndexOf('.');
			bool end = indexSeperator == -1;
			if (end && this.CompareKey(key)) {
				return true;
			}

			ReadOnlySpan<char> currentStep = end ? key : key[..indexSeperator];
			ReadOnlySpan<char> nextStep = end ? ReadOnlySpan<char>.Empty : key[++indexSeperator..];
			if (end) {
				for (int x = 0; x < this.InnerCount; x++) {
					NanoJson value = this.InnerValues[x];
					if (value.ContainsKey(currentStep)) {
						return true;
					}
				}
			}
			else {
				for (int x = 0; x < this.InnerCount; x++) {
					NanoJson value = this.InnerValues[x];
					if (value.ContainsKey(nextStep)) {
						return true;
					}
				}
			}

			return false;
		}

		public IEnumerator<NanoJson> GetEnumerator() {
			switch (this.Type) {
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
	}
}
