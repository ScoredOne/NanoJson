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

namespace NanoJson {

	public readonly ref struct nJson {
		public ref struct Enumerator {
			private readonly nJson owner;
			private int index;
			private nJson current;

			private int x;
			private int y;
			private readonly int len;
			private int debth;
			private int arrayPos;

			internal Enumerator(nJson owner) {
				this.owner = owner;
				this.index = -1;
				this.current = owner;

				this.len = owner.Value.Length;
				this.x = 0;
				this.y = -1;
				this.debth = 0;
				this.arrayPos = 0;
			}

			public readonly nJson Current => this.current;

			public bool MoveNext() {
				switch (owner.Type) {
					case JsonType.Object:
						if (x == len) {
							return false;
						}
						ReadOnlySpan<char> name;
						while (true) {
							while (owner.Value[x] != '"') { x++; }
							if (owner.Value[++x] == '"') {
								name = ReadOnlySpan<char>.Empty;
							}
							else {
								y = x;
								while (owner.Value[++x] != '"') { }
								name = owner.Value[y..x];
							}

							while (owner.Value[++x] != ':') { }
							while (char.IsWhiteSpace(owner.Value[++x])) { }
							y = x;
							while (x < len) {
								switch (owner.Value[x]) {
									case '"':
										while (owner.Value[++x] != '"') { }
										break;
									case '{':
									case '[':
										debth++;
										break;
									case ']':
										debth--;
										break;
									case '}':
										if (--debth < 0) { // no comma found, process last segment
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (debth == 0) {
											goto ProcessJsonObject;
										}
										break;
								}
								x++;
							}

							ProcessJsonObject:
							if (!name.IsEmpty) {
								this.current = new nJson(name, owner.Value[y..x]);
								return true;
							}
							else if (x == len) {
								return false;
							}
						}
					case JsonType.Array:
						this.index++;
						if (y == -1) {
							while (owner.Value[x++] != '[') { }
						}
						y = x;
						debth = 1;

						while (true) {
							while (true) {
								switch (owner.Value[x]) {
									case '"':
										while (owner.Value[++x] != '"') { }
										break;
									case '[':
									case '{':
										debth++;
										break;
									case '}':
										debth--;
										break;
									case ']':
										if (--debth == 0) {
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (debth == 1) {
											goto ProcessJsonObject;
										}
										break;
								}
								x++;
							}

							ProcessJsonObject:
							if (arrayPos == index) {
								this.current = new nJson(owner.Value[y..x]);
								return true;
							}
							else {
								y = ++x;
								if (x == len) {
									return false;
								}
								arrayPos++;
							}
						}
					default:
						if (this.index == -1) {
							this.index = 0;
							return true;
						}
						return false;
				}
			}

			public void Reset() {
				this.index = -1;
				this.x = 0;
				this.y = -1;
				this.debth = 0;
				this.arrayPos = 0;
			}
		}

		public readonly JsonType Type;
		public readonly ReadOnlySpan<char> Key;
		public readonly ReadOnlySpan<char> Value;
		public readonly bool IsEmpty;

		public readonly bool IsNothing => Key == ReadOnlySpan<char>.Empty && Value == ReadOnlySpan<char>.Empty;

		public nJson(string key, string value) : this(key.AsSpan(), value.AsSpan()) { }
		public nJson(Span<char> key, Span<char> value) : this((ReadOnlySpan<char>)key, (ReadOnlySpan<char>)value) { }
		public nJson(ReadOnlySpan<char> key, ReadOnlySpan<char> value) : this(value) {
			this.Key = key;
		}

		public nJson(string data) : this(data.AsSpan()) { }
		public nJson(Span<char> data) : this((ReadOnlySpan<char>)data) { }
		public nJson(ReadOnlySpan<char> data) {
			this.Value = data.Trim();
			this.Key = ReadOnlySpan<char>.Empty;
			if (this.Value.IsWhiteSpace()) {
				this.Type = JsonType.Null;
				this.IsEmpty = true;
				return;
			}

			int x = 0;
			char c = this.Value[0];
			while (char.IsWhiteSpace(c)) {
				c = this.Value[++x];
			}
			switch (c) {
				case '"': {
					this.Type = JsonType.String;
					int first = ++x;
					x = this.Value.Length - 1;

					while (true) {
						if (this.Value[x] == '"') {
							break;
						}
						x--;
					}

					if (x < first) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					if (first == x) {
						this.Value = string.Empty.AsSpan();
						this.IsEmpty = true;
						return;
					}
					this.Value = this.Value[first..x];
					this.IsEmpty = false;
					return;
				}
				case '[': {
					this.Type = JsonType.Array;
					int first = x;
					int len = this.Value.Length;

					while (true) {
						if (this.Value[--len] == ']') {
							break;
						}
					}

					if (len <= x) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					while (char.IsWhiteSpace(this.Value[++x])) { }
					if (x == len) {
						this.Value = this.Value[first..len];
						this.IsEmpty = true;
						return;
					}

					this.Value = this.Value[first..++len];
					this.IsEmpty = false;
					return;
				}
				case '{': {
					this.Type = JsonType.Object;
					int first = x;
					int len = this.Value.Length;

					while (true) {
						if (this.Value[--len] == '}') {
							break;
						}
					}

					if (len <= x) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					while (char.IsWhiteSpace(this.Value[++x])) { }
					if (x == len) {
						this.Value = this.Value[first..++len];
						this.IsEmpty = true;
						return;
					}

					this.Value = this.Value[first..len];
					this.IsEmpty = false;
					return;
				}
				case 't':
				case 'T': {
					this.Type = JsonType.Boolean;
					this.IsEmpty = false;
					if (this.Value.Length - x == 4) {
						c = this.Value[++x];
						if (c == 'r' || c == 'R') {
							c = this.Value[++x];
							if (c == 'u' || c == 'U') {
								c = this.Value[++x];
								if (c == 'e' || c == 'E') {
									this.Value = bool.TrueString.AsSpan();
									return;
								}
							}
						}
					}

					throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
				}
				case 'f':
				case 'F': {
					this.Type = JsonType.Boolean;
					this.IsEmpty = false;
					if (this.Value.Length - x == 5) {
						c = this.Value[++x];
						if (c == 'a' || c == 'A') {
							c = this.Value[++x];
							if (c == 'l' || c == 'L') {
								c = this.Value[++x];
								if (c == 's' || c == 'S') {
									c = this.Value[++x];
									if (c == 'e' || c == 'E') {
										this.Value = bool.FalseString.AsSpan();
										return;
									}
								}
							}
						}
					}

					throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
				}
				case 'n':
				case 'N': {
					this.Type = JsonType.Null;
					this.IsEmpty = false;
					if (this.Value.Length - x == 4) {
						c = this.Value[++x];
						if (c == 'u' || c == 'U') {
							c = this.Value[++x];
							if (c == 'l' || c == 'L') {
								c = this.Value[++x];
								if (c == 'l' || c == 'L') {
									this.Value = NanoJson.NULL.AsSpan();
									return;
								}
							}
						}
					}

					throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
				}
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
				case '9': {
					this.Type = JsonType.Number;
					this.IsEmpty = false;

					bool dec = false;
					bool E = false;

					int len = this.Value.Length;
					while (++x < len) {
						c = this.Value[x];
						if (c == '.') {
							if (dec
								|| !char.IsDigit(this.Value[++x])) {
								throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
							}
							dec = true;
							continue;
						}
						else if (c == 'e' || c == 'E') {
							if (E
								|| ((c = this.Value[++x]) != '+' && c != '-')
								|| !char.IsDigit(this.Value[++x])) {
								throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
							}
							E = true;
							continue;
						}
						else if (!char.IsDigit(c)) {
							throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
						}
					}

					return;
				}
			}
			throw new ArgumentException($"Parse failed (TryParse: {data.ToString()})", nameof(data));

		}

		public readonly nJson this[ReadOnlySpan<char> key]
		{
			get
			{
				switch (this.Type) {
					case JsonType.Object:
						int pathLen = key.Length;
						int nameLen = -1;

						int x = 0;
						int len = this.Value.Length;
						int y = 0;
						int debth = 0;

						bool found = false;

						while (true) {
							while (this.Value[x] != '"') { x++; }
							ReadOnlySpan<char> name;
							if (this.Value[++x] == '"') {
								name = ReadOnlySpan<char>.Empty;
							}
							else {
								y = x;
								while (this.Value[++x] != '"') { }
								name = this.Value[y..x];
							}

							if (key.StartsWith(name)) {
								nameLen = name.Length;
								found = true;
							}

							while (this.Value[++x] != ':') { }
							while (char.IsWhiteSpace(this.Value[++x])) { }
							y = x;
							while (true) {
								switch (this.Value[x]) {
									case '"':
										while (this.Value[++x] != '"') { }
										break;
									case '{':
									case '[':
										debth++;
										break;
									case ']':
										debth--;
										break;
									case '}':
										if (--debth < 0) { // no comma found, process last segment
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (debth == 0) {
											goto ProcessJsonObject;
										}
										break;
								}
								x++;
							}

							ProcessJsonObject:
							if (found) {
								if (nameLen == pathLen) {
									return new nJson(this.Value[y..x]);
								}
								else {
									return new nJson(this.Value[y..x])[key[++nameLen..]];
								}
							}
							else if (x == len) {
								throw new IndexOutOfRangeException();
							}
						}

						throw new ArgumentException($"Path provided was invalid [{key.ToString()}]", nameof(key));
					default:
						throw new InvalidOperationException();
				}
			}
		}

		public readonly nJson this[int index]
		{
			get
			{
				switch (this.Type) {
					case JsonType.Array:
						int x = 0;
						int len = this.Value.Length;
						while (this.Value[x++] != '[') { }
						int y = x;
						int debth = 1;
						int arrayPos = 0;

						while (true) {
							while (true) {
								switch (this.Value[x]) {
									case '"':
										while (this.Value[++x] != '"') { }
										break;
									case '[':
									case '{':
										debth++;
										break;
									case '}':
										debth--;
										break;
									case ']':
										if (--debth == 0) {
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (debth == 1) {
											goto ProcessJsonObject;
										}
										break;
								}
								x++;
							}

							ProcessJsonObject:
							if (arrayPos == index) {
								return new nJson(this.Value[y..x]);
							}
							else {
								y = ++x;
								if (x == len) {
									throw new IndexOutOfRangeException();
								}
								arrayPos++;
							}
						}
					default:
						throw new IndexOutOfRangeException();
				}
			}
		}

		public readonly Enumerator GetEnumerator() => new Enumerator(this);

		public string GetString => this.Value.ToString();

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public readonly double GetNumber => double.Parse(this.Value);


		/// <summary>
		/// Get if This object is Null
		/// </summary>
		public readonly bool IsNull => this.Type == JsonType.Null;

		/// <summary>
		/// Get the bool value of This object
		/// </summary>
		public readonly bool GetBool => bool.Parse(this.Value);
	}

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

	public readonly struct NanoJson : IEquatable<NanoJson> {
		public readonly struct NanoArray {
			public readonly static NanoArray Empty = new NanoArray(0);
			public readonly NanoJson[] Data;

			public NanoArray(int size) {
				this.Data = new NanoJson[size];
			}

			public NanoArray(params NanoJson[] data) {
				this.Data = data;
			}

			public ref NanoJson this[int index]
			{
				get => ref Data[index];
			}
		}

		public ref struct Enumerator {
			private readonly NanoJson owner;
			private int index;

			internal Enumerator(NanoJson owner) {
				this.owner = owner;
				this.index = -1;
			}

			public readonly NanoJson Current => owner.InnerValues[this.index];

			public bool MoveNext() {
				switch (owner.Type) {
					case JsonType.Object:
					case JsonType.Array:
						if (++this.index < owner.InnerCount) {
							return true;
						}
						return false;
					default:
						if (this.index == -1) {
							this.index = 0;
							return true;
						}
						return false;
				}
			}

			public void Reset() => this.index = -1;
		}


		private const string INDENT_TABS = "   "; // Can be better, indenting seems off
		private const int INDENT_LEN = 3;
		public const string NULL = "null";

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

		public static NanoJson Pin(nJson data) { // Needs testing
			if (data.IsNothing) {
				return NanoJson.Empty;
			}
			if (data.Key == ReadOnlySpan<char>.Empty) {
				if (data.IsNull) {
					return NanoJson.Empty;
				} else {
					return NanoJson.ParseJson(data.Value.ToString());
				}
			}
			return NanoJson.ParseJson(data.Key.ToString(), data.Value.ToString());
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

		public readonly JsonType Type;
		private readonly NanoArray InnerValues;
		private readonly ReadOnlyMemory<char> ReferenceData;
		private readonly ReadOnlyMemory<char> KeyData;
		private readonly int InnerCount;

		private NanoJson(ReadOnlyMemory<char> key, ReadOnlyMemory<char> data, bool literal = false, int innerLength = -1, int knownLen = -1) : this(data, literal, innerLength, knownLen) {
			this.KeyData = key;
		}

		private NanoJson(ReadOnlyMemory<char> reference, bool literal = false, int innerLength = -1, int knownLen = -1) {
			this.KeyData = ReadOnlyMemory<char>.Empty;
			this.InnerCount = innerLength;
			if (literal) {
				this.Type = JsonType.String;
				this.InnerValues = NanoArray.Empty;
				this.ReferenceData = reference;
			}
			else {
				if (reference.IsEmpty) {
					this.Type = JsonType.Null;
					this.ReferenceData = ReadOnlyMemory<char>.Empty;
					this.InnerValues = NanoArray.Empty;
					return;
				}
				ReadOnlySpan<char> data = reference.Span;
				int x = 0;
				int len = knownLen == -1 ? data.Length : knownLen;
				char c = data[0];
				while (char.IsWhiteSpace(c)) {
					c = data[++x];
				}
				this.ReferenceData = reference[x..];
				switch (c) {
					case '"': {
						this.Type = JsonType.String;
						this.InnerValues = NanoArray.Empty;
						int first = ++x;
						x = len - 1;

						while (true) {
							if (data[x] == '"') {
								break;
							}
							x--;
						}

						if (x < first) {
							throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
						}
						if (first == x) {
							this.ReferenceData = string.Empty.AsMemory();
							return;
						}
						this.ReferenceData = reference[first..x];
						return;
					}
					case '[': {
						this.Type = JsonType.Array;
						int first = x;

						while (true) {
							if (data[--len] == ']') {
								break;
							}
						}

						if (len <= x) {
							throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
						}
						while (char.IsWhiteSpace(data[++x])) { }
						if (x == len) {
							this.InnerCount = 0;
							this.InnerValues = NanoArray.Empty;
							return;
						}
						len++;
						x = first;
						int debth = 0;

						if (this.InnerCount == -1) {
							int items = 1;

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
										if (--debth == 0) {
											goto Fin;
										}
										break;
									case ',':
										if (debth == 1) {
											items++;
										}
										break;
								}
								x++;
							}
							Fin:
							this.InnerCount = items; // Cant set this value from methods
						}
						this.InnerValues = new NanoArray(this.InnerCount);

						this.ProcessJsonArray(reference[first..len]);
						return;
					}
					case '{': {
						this.Type = JsonType.Object;
						int first = x;

						while (true) {
							if (data[--len] == '}') {
								break;
							}
						}

						if (len <= x) {
							throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
						}
						while (char.IsWhiteSpace(data[++x])) { }
						if (x == len) {
							this.InnerCount = 0;
							this.InnerValues = NanoArray.Empty;
							return;
						}
						len++;
						x = first;
						int debth = 0;
						if (this.InnerCount == -1) {
							int items = 1;
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
										if (--debth == 0) {
											goto Fin;
										}
										break;
									case ',':
										if (debth == 1) {
											items++;
										}
										break;
								}
								x++;
							}
							Fin:
							this.InnerCount = items; // Cant set this value from methods
						}
						this.InnerValues = new NanoArray(this.InnerCount);

						this.ProcessJsonObject(reference[first..len]);
						return;
					}
					case 't':
					case 'T': {
						this.Type = JsonType.Boolean;
						this.InnerValues = NanoArray.Empty;
						if (len - x == 4) {
							c = data[++x];
							if (c == 'r' || c == 'R') {
								c = data[++x];
								if (c == 'u' || c == 'U') {
									c = data[++x];
									if (c == 'e' || c == 'E') {
										this.ReferenceData = bool.TrueString.AsMemory();
										return;
									}
								}
							}
						}

						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
					}
					case 'f':
					case 'F': {
						this.Type = JsonType.Boolean;
						this.InnerValues = NanoArray.Empty;
						if (len - x == 5) {
							c = data[++x];
							if (c == 'a' || c == 'A') {
								c = data[++x];
								if (c == 'l' || c == 'L') {
									c = data[++x];
									if (c == 's' || c == 'S') {
										c = data[++x];
										if (c == 'e' || c == 'E') {
											this.ReferenceData = bool.FalseString.AsMemory();
											return;
										}
									}
								}
							}
						}

						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
					}
					case 'n':
					case 'N': {
						this.Type = JsonType.Null;
						this.InnerValues = NanoArray.Empty;
						if (len - x == 4) {
							c = data[++x];
							if (c == 'u' || c == 'U') {
								c = data[++x];
								if (c == 'l' || c == 'L') {
									c = data[++x];
									if (c == 'l' || c == 'L') {
										this.ReferenceData = NULL.AsMemory();
										return;
									}
								}
							}
						}

						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
					}
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
					case '9': {
						this.Type = JsonType.Number;
						this.InnerValues = NanoArray.Empty;

						bool dec = false;
						bool E = false;

						while (++x < len) {
							c = data[x];
							if (c == '.') {
								if (dec
									|| !char.IsDigit(data[++x])) {
									throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
								}
								dec = true;
								continue;
							}
							else if (c == 'e' || c == 'E') {
								if (E
									|| ((c = data[++x]) != '+' && c != '-')
									|| !char.IsDigit(data[++x])) {
									throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
								}
								E = true;
								continue;
							}
							else if (!char.IsDigit(c)) {
								throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
							}
						}

						return;
					}
				}
				throw new ArgumentException($"Parse failed (TryParse: {data.ToString()})", nameof(reference));
			}
		}

		private NanoJson(JsonType type, params NanoJson[] contents) : this(ReadOnlyMemory<char>.Empty, type, contents) { }

		private NanoJson(string key, JsonType type, params NanoJson[] contents) : this(key.AsMemory(), type, contents) { }

		private NanoJson(ReadOnlyMemory<char> key, JsonType type, params NanoJson[] contents) {
			switch (type) {
				case JsonType.Array:
				case JsonType.Object:
					this.Type = type;
					this.InnerValues = new NanoArray(contents);
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
			this.ReferenceData = value ? bool.TrueString.AsMemory() : bool.FalseString.AsMemory();
			this.InnerValues = NanoArray.Empty;
			this.InnerCount = -1;
		}

		private NanoJson(string key, double value) : this(key.AsMemory(), value) { }
		private NanoJson(ReadOnlyMemory<char> key, double value) {
			this.KeyData = key;
			this.Type = JsonType.Number;
			this.ReferenceData = value.ToString().AsMemory();
			this.InnerValues = NanoArray.Empty;
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
		private void ProcessJsonArray(ReadOnlyMemory<char> reference) {
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
						case ']':
							if (--debth == 0) {
								goto ProcessJsonObject;
							}
							break;
						case ',':
							if (debth == 2) {
								innerSize++;
							}
							else if (debth == 1) {
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
		private void ProcessJsonObject(ReadOnlyMemory<char> reference) {
			ReadOnlySpan<char> data = reference.Span;

			int x = 0;
			int y = 0;
			int debth = 0;
			int index = 0;
			int innerSize;

			while (true) {
				while (data[x] != '"') { x++; }
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
						case ']':
							debth--;
							break;
						case '}':
							if (--debth < 0) { // no comma found, process last segment
								goto ProcessJsonObject;
							}
							break;
						case ',':
							if (debth == 1) {
								innerSize++;
							}
							else if (debth == 0) {
								goto ProcessJsonObject;
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

		public readonly override string ToString() => this.ToString(false);

		public readonly string ToString(bool pretty) {
			int count = 0;
			this.CalculateStringSize(pretty, ref count);

			char[] buffer = ArrayPool<char>.Shared.Rent(count);
			Memory<char> bufferMemory = buffer.AsMemory()[..count];
			this.ProcessString(false, pretty, in bufferMemory);

			string builtString = new string(bufferMemory.Span);
			ArrayPool<char>.Shared.Return(buffer);
			return builtString;
		}

		private readonly void CalculateStringSize(bool pretty, ref int count) {
			int indent = 0;
			this.CalculateStringSize(false, pretty, ref count, ref indent);
		}

		private readonly void CalculateStringSize(bool AsValue, bool pretty, ref int count, ref int indent) {
			NanoJson value;
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
						count += indent * INDENT_LEN;
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
						count += this.InnerCount * ((indent * INDENT_LEN) + 6);
						for (int x = 0; x < this.InnerCount; x++) {
							value = this.InnerValues[x];
							count += value.KeyData.Length;
							value.CalculateStringSize(true, pretty, ref count, ref indent);
						}
					}
					else {
						count += (this.InnerCount * 5) - 1;
						for (int x = 0; x < this.InnerCount; x++) {
							value = this.InnerValues[x];
							count += value.KeyData.Length;
							value.CalculateStringSize(true, pretty, ref count, ref indent);
						}
					}

					indent--;
					if (pretty) {
						count += indent * INDENT_LEN;
					}
					count++;
					break;
				case JsonType.Array:
					if (pretty && !AsValue) {
						count += indent * INDENT_LEN;
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
							count += this.InnerCount * 2;
							for (int x = 0; x < this.InnerCount; x++) {
								value = this.InnerValues[x];
								switch (value.Type) {
									case JsonType.Null:
									case JsonType.String:
									case JsonType.Number:
									case JsonType.Boolean:
										count += indent * INDENT_LEN;
										break;
								}
								value.CalculateStringSize(false, pretty, ref count, ref indent);
							}
						}
						else {
							count += (this.InnerCount * 2) - 1;
							for (int x = 0; x < this.InnerCount; x++) {
								value = this.InnerValues[x];
								value.CalculateStringSize(false, pretty, ref count, ref indent);
							}
						}
						indent--;
						if (pretty) {
							count += indent * INDENT_LEN;
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
		private readonly void ProcessString(bool AsValue, bool pretty, in Memory<char> sb) {
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
		private readonly void ProcessString(bool AsValue, bool pretty, ref int indent, in Memory<char> sb, ref int sbPos) {
			Span<char> data = sb.Span;
			switch (this.Type) {
				case JsonType.String: {
					ReadOnlySpan<char> refStringSpan = this.ReferenceData.Span;
					data[sbPos++] = '"';
					refStringSpan.CopyTo(data[sbPos..(sbPos += refStringSpan.Length)]);
					data[sbPos++] = '"';
					break;
				}
				case JsonType.Null:
				case JsonType.Number:
				case JsonType.Boolean: {
					ReadOnlySpan<char> refSpan = this.ReferenceData.Span;
					refSpan.CopyTo(data[sbPos..(sbPos += refSpan.Length)]);
					break;
				}
				case JsonType.Object: {
					ReadOnlySpan<char> indentSpan = pretty ? INDENT_TABS.AsSpan() : ReadOnlySpan<char>.Empty;
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
					int limit = this.InnerCount - 1;
					ReadOnlySpan<char> keySpan;
					NanoJson value;
					if (pretty) {
						data[sbPos++] = '\n';
						int x = 0;
						for (; x < limit; x++) {
							value = this.InnerValues[x];
							for (int y = 0; y < indent; y++) {
								indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
							}
							data[sbPos++] = '"';
							keySpan = value.KeyData.Span;
							keySpan.CopyTo(data[sbPos..(sbPos += keySpan.Length)]);
							data[sbPos++] = '"';
							data[sbPos++] = ':';
							data[sbPos++] = ' ';
							value.ProcessString(true, pretty, ref indent, in sb, ref sbPos);
							data[sbPos++] = ',';
							data[sbPos++] = '\n';
						}
						value = this.InnerValues[x];
						for (int y = 0; y < indent; y++) {
							indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
						}
						data[sbPos++] = '"';
						keySpan = value.KeyData.Span;
						keySpan.CopyTo(data[sbPos..(sbPos += keySpan.Length)]);
						data[sbPos++] = '"';
						data[sbPos++] = ':';
						data[sbPos++] = ' ';
						value.ProcessString(true, pretty, ref indent, in sb, ref sbPos);
						data[sbPos++] = '\n';
					}
					else {
						int x = 0;
						for (; x < limit; x++) {
							value = this.InnerValues[x];
							data[sbPos++] = '"';
							keySpan = value.KeyData.Span;
							keySpan.CopyTo(data[sbPos..(sbPos += keySpan.Length)]);
							data[sbPos++] = '"';
							data[sbPos++] = ':';
							data[sbPos++] = ' ';
							value.ProcessString(true, pretty, ref indent, in sb, ref sbPos);
							data[sbPos++] = ',';
						}
						value = this.InnerValues[x];
						data[sbPos++] = '"';
						keySpan = value.KeyData.Span;
						keySpan.CopyTo(data[sbPos..(sbPos += keySpan.Length)]);
						data[sbPos++] = '"';
						data[sbPos++] = ':';
						data[sbPos++] = ' ';
						value.ProcessString(true, pretty, ref indent, in sb, ref sbPos);
					}

					indent--;
					if (pretty) {
						for (int x = 0; x < indent; x++) {
							indentSpan.CopyTo(data[sbPos..(sbPos += INDENT_LEN)]);
						}
					}
					data[sbPos++] = '}';
					break;
				}
				case JsonType.Array: {
					ReadOnlySpan<char> indentSpan = pretty ? INDENT_TABS.AsSpan() : ReadOnlySpan<char>.Empty;
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
						int limit = this.InnerCount - 1;
						NanoJson value;
						if (pretty) {
							data[sbPos++] = '\n';
							int x = 0;
							for (; x < limit; x++) {
								value = this.InnerValues[x];
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
								data[sbPos++] = ',';
								data[sbPos++] = '\n';
							}
							value = this.InnerValues[x];
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
							data[sbPos++] = '\n';
						}
						else {
							int x = 0;
							for (; x < limit; x++) {
								value = this.InnerValues[x];
								value.ProcessString(false, pretty, ref indent, in sb, ref sbPos);
								data[sbPos++] = ',';
							}
							value = this.InnerValues[x];
							value.ProcessString(false, pretty, ref indent, in sb, ref sbPos);
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
		}

		public string GetString => this.ReferenceData.ToString();

		/// <summary>
		/// Get the data used inside This object
		/// </summary>
		public readonly ReadOnlySpan<char> GetSpan => this.ReferenceData.Span;

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public readonly double GetNumber => double.Parse(this.ReferenceData.Span);


		/// <summary>
		/// Get the values contained inside This object but as a new array
		/// </summary>
		public NanoJson[] GetCopyOfContainedArray
		{
			get
			{
				NanoJson[] copy = new NanoJson[this.InnerCount];
				this.InnerValues.Data.CopyTo(copy, 0);
				return copy;
			}
		}

		/// <summary>
		/// Get if This object is Null
		/// </summary>
		public readonly bool IsNull => this.Type == JsonType.Null;

		/// <summary>
		/// Get the bool value of This object
		/// </summary>
		public readonly bool GetBool => bool.Parse(this.ReferenceData.Span);

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
		public readonly bool CompareKey(string key) => this.CompareKey(key.AsSpan());
		/// <summary>
		/// Compare the key of This object
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public readonly bool CompareKey(ReadOnlySpan<char> key) {
			return MemoryExtensions.SequenceEqual(key, KeyData.Span);
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key Found</returns>
		public readonly bool ContainsKey(string key, out NanoJson found) => this.ContainsKey(key.AsSpan(), out found);
		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key Found</returns>
		public readonly bool ContainsKey(ReadOnlySpan<char> key, out NanoJson found) {
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

		public readonly Enumerator GetEnumerator() => new Enumerator(this);

		public readonly override bool Equals(object obj) => obj is NanoJson other && this.Equals(other);
		public readonly bool Equals(NanoJson other) {
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

		public readonly override int GetHashCode() {
			return HashCode.Combine(this.Type, this.InnerValues, this.ReferenceData, this.KeyData, this.InnerCount);
		}
	}
}
