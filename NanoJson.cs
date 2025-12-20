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
using System.Runtime.CompilerServices;

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
				this.current = nJson.Empty;

				this.len = owner.Value.Length;
				this.x = 0;
				this.y = -1;
				this.debth = 0;
				this.arrayPos = 0;
			}

			public readonly nJson Current => this.current;

			public bool TryGetIndex(int index, out nJson value) {
				if (index < 0) {
					throw new ArgumentOutOfRangeException(nameof(index));
				}
				if (index == this.index) {
					value = this.current;
					return true;
				}
				if (this.index > index) {
					this.Reset();
				}
				bool found = false;
				while (this.MoveNext()) {
					if (this.index == index) {
						found = true;
						break;
					}
				}
				if (found) {
					value = this.current;
					return found;
				}
				else {
					value = nJson.Empty;
					return found;
				}
			}

			public bool MoveNext() {
				switch (this.owner.Type) {
					case JsonType.Object:
						if (this.owner.IsEmpty) {
							return false;
						}
						while (++this.y < this.len) {
							switch (this.owner.Value[this.y]) {
								case '"':
									goto go;
								case '}':
									return false;
							}
						}
						if (this.y == this.len) {
							return false;
						}
						go:
						this.x = this.y;
						ReadOnlySpan<char> name;
						while (true) {
							if (this.owner.Value[++this.x] == '"') {
								name = ReadOnlySpan<char>.Empty;
							}
							else {
								this.y = this.x;
								while (this.owner.Value[++this.x] != '"') { }
								name = this.owner.Value[this.y..this.x];
							}

							while (this.owner.Value[++this.x] != ':') { }
							while (char.IsWhiteSpace(this.owner.Value[++this.x])) { }
							this.y = this.x;
							this.index++;
							while (true) {
								switch (this.owner.Value[this.x]) {
									case '"':
										while (this.owner.Value[++this.x] != '"') { }
										break;
									case '{':
									case '[':
										this.debth++;
										break;
									case ']':
										this.debth--;
										break;
									case '}':
										if (--this.debth < 0) { // no comma found, process last segment
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (this.debth == 0) {
											goto ProcessJsonObject;
										}
										break;
								}
								this.x++;
							}

							ProcessJsonObject:
							if (!name.IsEmpty) {
								this.current = new nJson(name, this.owner.Value[this.y..this.x]);
								this.y = this.x;
								return true;
							}
							else if (++this.x == this.len) {
								return false;
							}
						}
					case JsonType.Array:
						if (this.owner.IsEmpty) {
							return false;
						}
						this.index++;
						if (this.y == -1) {
							while (this.owner.Value[this.x++] != '[') { }
						}
						this.y = this.x;
						this.debth = 1;

						while (true) {
							while (true) {
								switch (this.owner.Value[this.x]) {
									case '"':
										while (this.owner.Value[++this.x] != '"') { }
										break;
									case '[':
									case '{':
										this.debth++;
										break;
									case '}':
										this.debth--;
										break;
									case ']':
										if (--this.debth == 0) {
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (this.debth == 1) {
											goto ProcessJsonObject;
										}
										break;
								}
								this.x++;
							}

							ProcessJsonObject:
							if (this.arrayPos == this.index) {
								this.current = new nJson(this.owner.Value[this.y..this.x]);
								return true;
							}
							else {
								if (++this.x == this.len) {
									return false;
								}
								this.y = this.x;
								this.arrayPos++;
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

		public static nJson Empty => new nJson(true);

		public readonly JsonType Type;
		public readonly ReadOnlySpan<char> Key;
		public readonly ReadOnlySpan<char> Value;
		public readonly bool IsEmpty;

		public readonly bool IsNothing => this.Key == ReadOnlySpan<char>.Empty && this.Value == ReadOnlySpan<char>.Empty;

		private nJson(bool _) {
			this.Type = JsonType.Null;
			this.Key = ReadOnlySpan<char>.Empty;
			this.Value = ReadOnlySpan<char>.Empty;
			this.IsEmpty = true;
		}

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
					if (x == len++) {
						this.Value = this.Value[first..len];
						this.IsEmpty = true;
						return;
					}

					this.Value = this.Value[first..len];
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
					if (x == len++) {
						this.Value = this.Value[first..len];
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
									this.Value = NJson.NULL.AsSpan();
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
				if (this.IsEmpty) {
					throw new IndexOutOfRangeException("Body is Empty");
				}
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
								throw new ArgumentException($"Path provided was invalid [{key.ToString()}]", nameof(key));
							}
						}
					default:
						throw new InvalidOperationException();
				}
			}
		}

		public bool TryGetKey(ReadOnlySpan<char> key, out nJson value) {
			if (this.Type != JsonType.Object) {
				value = nJson.Empty;
				return false;
			}
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
						value = new nJson(this.Value[y..x]);
						return true;
					}
					else {
						if (new nJson(this.Value[y..x]).TryGetKey(key[++nameLen..], out value)) {
							return true;
						}
						return false;
					}
				}
				else if (x == len) {
					value = nJson.Empty;
					return false;
				}
			}
		}

		public readonly nJson this[int index]
		{
			get
			{
				if (this.IsEmpty) {
					throw new IndexOutOfRangeException("Body is Empty");
				}
				switch (this.Type) {
					case JsonType.Object: {
						int len = this.Value.Length;
						int x = 0;
						int y = 0;
						int debth = 0;
						int currentIndex = -1;
						ReadOnlySpan<char> name;
						while (true) {
							while (this.Value[x] != '"') { x++; }
							if (this.Value[++x] == '"') {
								name = ReadOnlySpan<char>.Empty;
							}
							else {
								y = x;
								while (this.Value[++x] != '"') { }
								name = this.Value[y..x];
							}

							while (this.Value[++x] != ':') { }
							while (char.IsWhiteSpace(this.Value[++x])) { }
							y = x;
							currentIndex++;
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
							if (currentIndex == index) {
								return new nJson(name, this.Value[y..x]);
							}
							else if (++x == len) {
								throw new IndexOutOfRangeException();
							}
						}
					}
					case JsonType.Array: {
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
								if (++x == len) {
									throw new IndexOutOfRangeException();
								}
								y = x;
								arrayPos++;
							}
						}
					}
					default:
						throw new IndexOutOfRangeException();
				}
			}
		}

		public readonly Enumerator GetEnumerator() => new Enumerator(this);

		/// <summary>
		/// Get the string value as-is in relation to this object
		/// </summary>
		public string GetStringLiteral => this.Value.ToString();

		/// <summary>
		/// Get the decoded string value of the object
		/// </summary>
		public string GetStringDecoded
		{
			get
			{
				char c;
				int len = this.Value.Length;
				char[] buffer = ArrayPool<char>.Shared.Rent(len);
				int x = 0;
				int y = 0;
				while (x < len) {
					c = this.Value[x];
					switch (c) {
						case '\\':
							c = this.Value[++x];
							switch (c) {
								case '\\':
								case '/':
								case '"':
									buffer[y++] = c;
									break;
								case 'f':
									buffer[y++] = '\f';
									break;
								case 'b':
									buffer[y++] = '\b';
									break;
								case 'n':
									buffer[y++] = '\n';
									break;
								case 'r':
									buffer[y++] = '\r';
									break;
								case 't':
									buffer[y++] = '\t';
									break;
								case 'u': {
									buffer[y++] = (char)((NJson.ReadHexNumber(this.Value[++x]) * 4096)
										+ (NJson.ReadHexNumber(this.Value[++x]) * 256)
										+ (NJson.ReadHexNumber(this.Value[++x]) * 16)
										+ NJson.ReadHexNumber(this.Value[++x]));
									break;
								}
							}
							break;
						default:
							buffer[y++] = c;
							break;
					}
					x++;
				}
				string decodedValue = buffer.AsSpan()[..y].ToString();
				ArrayPool<char>.Shared.Return(buffer);
				return decodedValue;
			}
		}

		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public string TryGetString(ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(key, out nJson value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetString(ReadOnlySpan<char> key, out string @out, bool decoded = true) {
			if (this.TryGetKey(key, out nJson value) && value.Type == JsonType.String) {
				@out = decoded ? value.GetStringDecoded : value.GetStringLiteral;
				return true;
			}
			else {
				@out = string.Empty;
				return false;
			}
		}

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public readonly double GetNumber => double.TryParse(this.Value, out double value) ? value : double.NaN;

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public readonly T GetNumberOfType<T>() where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			return double.TryParse(this.Value, out double value) ? NJson.Number.GetValue<T>(value) : NJson.Number.GetEmpty<T>();
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
		public double TryGetNumber(ReadOnlySpan<char> key) => this.TryGetKey(key, out nJson value) ? value.GetNumber : double.NaN;
		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetNumber(ReadOnlySpan<char> key, out double @out) {
			if (this.TryGetKey(key, out nJson value) && value.Type == JsonType.Number) {
				return double.TryParse(this.Value, out @out);
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
		public T TryGetNumber<T>(ReadOnlySpan<char> key) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			this.TryGetNumber(key, out T value);
			return value;
		}
		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetNumber<T>(ReadOnlySpan<char> key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			if (this.TryGetKey(key, out nJson value) && value.Type == JsonType.Number) {
				@out = value.GetNumberOfType<T>();
				return true;
			}
			else {
				@out = NJson.Number.GetEmpty<T>();
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
		public bool TryGetBool(string key) => this.TryGetKey(key, out nJson value) && value.GetBool;

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
		public bool TryGetBool(ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(key, out nJson value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

		/// <summary>
		/// Get if This object is Null
		/// </summary>
		public readonly bool IsNull => this.Type == JsonType.Null;

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

	public readonly struct NJson : IEquatable<NJson> {
		public readonly struct NanoArray {
			public readonly static NanoArray Empty = new NanoArray(false);
			public readonly NJson[] Data;

			private NanoArray(bool _) {
				this.Data = Array.Empty<NJson>();
			}

			public NanoArray(int size) {
				this.Data = new NJson[size];
			}

			public NanoArray(params NJson[] data) {
				this.Data = data;
			}

			public ref NJson this[int index]
			{
				get => ref this.Data[index];
			}

			public readonly int Length => this.Data.Length;

			public readonly Enumerator GetEnumerator() => new Enumerator(this);

			public readonly NJson[] Clone() => this.Length == 0 ? Empty.Data : (NJson[])this.Data.Clone();

			public readonly ReadOnlySpan<NJson> GetSpan => this.Data.AsSpan();

			public ref struct Enumerator {
				private readonly NanoArray owner;
				private int index;

				public readonly NJson Current => this.owner[this.index];

				public Enumerator(NanoArray owner) {
					this.owner = owner;
					this.index = -1;
				}

				public bool MoveNext() => ++this.index < this.owner.Length;

				public void Reset() => this.index = -1;
			}
		}

		public ref struct Enumerator {
			private readonly NJson owner;
			private readonly int innerCount;
			private int index;

			internal Enumerator(NJson owner) {
				this.owner = owner;
				this.index = -1;
				this.innerCount = owner.InnerValues.Length;
			}

			public readonly NJson Current => this.owner.InnerValues[this.index];

			public bool MoveNext() {
				switch (this.owner.Type) {
					case JsonType.Object:
					case JsonType.Array:
						return ++this.index < this.innerCount;
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

		public static NJson ParseJson(string key, string data) {
			return NJson.ParseJson(key.AsMemory(), data.AsMemory());
		}

		private static NJson ParseJson(ReadOnlyMemory<char> key, ReadOnlyMemory<char> data) {
			if (key.IsEmpty) {
				return NJson.ParseJson(data);
			}
			else {
				return new NJson(key, data, false, -1);
			}
		}

		public static NJson ParseJson(string data) {
			return NJson.ParseJson(data.AsMemory());
		}

		private static NJson ParseJson(ReadOnlyMemory<char> data) {
			return new NJson(data, false, -1);
		}

		public static NJson Pin(nJson data) {
			if (data.IsNothing) {
				return NJson.Empty;
			}
			if (data.Key == ReadOnlySpan<char>.Empty) {
				if (data.IsNull) {
					return NJson.Empty;
				}
				else {
					return NJson.ParseJson(data.Value.ToString());
				}
			}
			return NJson.ParseJson(data.Key.ToString(), data.Value.ToString());
		}

		/// <summary>
		/// Remakes the existing NJson object, usually an internal node, by allocating the segments to arrays. Typically needed when you want to deallocate a large Json string container but keeping its smaller internal nodes
		/// </summary>
		/// <param name="data"></param>
		/// <returns></returns>
		/// <exception cref="NotSupportedException"></exception>
		public static NJson Pin(NJson data) {
			switch (data.Type) {
				case JsonType.Null:
					if (data.KeyData.IsEmpty) {
						return NJson.Empty;
					} else {
						return new NJson(data.KeyData.ToArray());
					}
				case JsonType.Object:
				case JsonType.Array:
					return new NJson(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray());
				case JsonType.String:
					return new NJson(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray(), true);
				case JsonType.Number:
					return new NJson(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, double.Parse(data.ReferenceData.Span));
				case JsonType.Boolean:
					return new NJson(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, bool.Parse(data.ReferenceData.Span));
				default:
					throw new NotSupportedException();
			}
		}

		public static NJson CreateArray(string key, NJson[] data) {
			return NJson.CreateArray(key.AsMemory(), data);
		}

		private static NJson CreateArray(ReadOnlyMemory<char> key, NJson[] data) {
			return new NJson(key, JsonType.Array, data);
		}

		public static NJson CreateArray(NJson[] data) {
			return new NJson(JsonType.Array, data);
		}

		public static NJson CreateObject(string key, NJson[] data) {
			return NJson.CreateObject(key, data);
		}

		private static NJson CreateObject(ReadOnlyMemory<char> key, NJson[] data) {
			return new NJson(key, JsonType.Object, data);
		}

		public static NJson CreateObject(NJson[] data) {
			return new NJson(JsonType.Object, data);
		}

		public static NJson CreateStringObject(string key, string data) {
			return NJson.CreateStringObject(key.AsMemory(), data.AsMemory());
		}

		private static NJson CreateStringObject(ReadOnlyMemory<char> key, ReadOnlyMemory<char> data) {
			return new NJson(key, data, true, -1);
		}

		public static NJson ContainValueInObject(string key, NJson data) {
			return NJson.ContainValueInObject(key.AsMemory(), data);
		}

		private static NJson ContainValueInObject(ReadOnlyMemory<char> key, NJson data) {
			return new NJson(key, data);
		}

		public static NJson CreateBoolObject(string key, bool data) {
			return NJson.CreateBoolObject(key.AsMemory(), data);
		}

		private static NJson CreateBoolObject(ReadOnlyMemory<char> key, bool data) {
			return new NJson(key, data);
		}

		public static NJson CreateNumberObject(string key, double data) {
			return NJson.CreateNumberObject(key.AsMemory(), data);
		}

		private static NJson CreateNumberObject(ReadOnlyMemory<char> key, double data) {
			return new NJson(key, data);
		}

		public static NJson CreateNullObject(string key) {
			return NJson.CreateNullObject(key.AsMemory());
		}

		private static NJson CreateNullObject(ReadOnlyMemory<char> key) {
			return new NJson(key);
		}

		public readonly static NJson Empty = new NJson();

		public readonly JsonType Type;
		private readonly NanoArray InnerValues;
		private readonly ReadOnlyMemory<char> ReferenceData;
		private readonly ReadOnlyMemory<char> KeyData;

		private NJson(ReadOnlyMemory<char> key, ReadOnlyMemory<char> data, bool literal = false, int innerLength = -1, int knownLen = -1) : this(data, literal, innerLength, knownLen) {
			this.KeyData = key;
		}

		private NJson(ReadOnlyMemory<char> reference, bool literal = false, int innerLength = -1, int knownLen = -1) {
			this.KeyData = ReadOnlyMemory<char>.Empty;
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
							this.InnerValues = NanoArray.Empty;
							return;
						}
						len++;
						x = first;
						int debth = 0;

						if (innerLength == -1) {
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
							innerLength = items; // Cant set this value from methods
						}
						this.InnerValues = new NanoArray(innerLength);

						this.ProcessJsonArray(reference[first..len], innerLength);
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
							this.InnerValues = NanoArray.Empty;
							return;
						}
						len++;
						x = first;
						int debth = 0;
						if (innerLength == -1) {
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
							innerLength = items; // Cant set this value from methods
						}
						this.InnerValues = new NanoArray(innerLength);

						this.ProcessJsonObject(reference[first..len], innerLength);
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

		private NJson(JsonType type, params NJson[] contents) : this(ReadOnlyMemory<char>.Empty, type, contents) { }

		private NJson(string key, JsonType type, params NJson[] contents) : this(key.AsMemory(), type, contents) { }

		private NJson(ReadOnlyMemory<char> key, JsonType type, params NJson[] contents) {
			switch (type) {
				case JsonType.Null:
					this.Type = type;
					this.InnerValues = NanoArray.Empty;
					this.KeyData = key;
					this.ReferenceData = ReadOnlyMemory<char>.Empty;
					break;
				case JsonType.Array:
				case JsonType.Object:
					this.Type = type;
					this.InnerValues = new NanoArray(contents);
					this.KeyData = key;
					this.ReferenceData = ReadOnlyMemory<char>.Empty;
					break;
				default:
					throw new NotSupportedException();
			}
		}

		private NJson(string key, NJson value) : this(key.AsMemory(), value) { }
		private NJson(ReadOnlyMemory<char> key, NJson value) {
			this.KeyData = key;
			this.Type = value.Type;
			this.ReferenceData = value.ReferenceData;
			this.InnerValues = value.InnerValues;
		}

		private NJson(string key, bool value) : this(key.AsMemory(), value) { }
		private NJson(ReadOnlyMemory<char> key, bool value) {
			this.KeyData = key;
			this.Type = JsonType.Boolean;
			this.ReferenceData = value ? bool.TrueString.AsMemory() : bool.FalseString.AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(string key, double value) : this(key.AsMemory(), value) { }
		private NJson(ReadOnlyMemory<char> key, double value) {
			this.KeyData = key;
			this.Type = JsonType.Number;
			this.ReferenceData = value.ToString().AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(string key) : this(key.AsMemory()) { }
		private NJson(ReadOnlyMemory<char> key) {
			this.KeyData = key;
			this.Type = JsonType.Null;
			this.ReferenceData = ReadOnlyMemory<char>.Empty;
			this.InnerValues = NanoArray.Empty;
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key">Key or Path to desired value</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException">Key was not found in object</exception>
		/// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
		public NJson this[string path] => this[path.AsSpan()];

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key">Key or Path to desired value</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException">Key was not found in object</exception>
		/// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
		public NJson this[ReadOnlySpan<char> key]
		{
			get
			{
				switch (this.Type) {
					case JsonType.Object:
						if (this.TryGetKey(key, out NJson found)) {
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
		public NJson this[int index]
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
		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		private void ProcessJsonArray(ReadOnlyMemory<char> reference, int innerCount) {
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
				this.InnerValues[index++] = new NJson(reference[y..x], false, innerSize, x - y);
				if (index == innerCount) {
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
		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		private void ProcessJsonObject(ReadOnlyMemory<char> reference, int innerCount) {
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
				this.InnerValues[index++] = new NJson(name, reference[y..x], false, innerSize, x - y);
				if (index == innerCount) {
					return;
				}
			}
		}

		[Flags]
		public enum ToStringFormat : byte {
   None = 0,
			Pretty = 0x1,
			Decoded = 0x2,

			All = Pretty + Decoded,
		}

		public readonly override string ToString() => this.ToString(ToStringFormat.All);

		public readonly string ToString(in ToStringFormat format) {
			int count = 0;
			bool pretty = format.HasFlag(ToStringFormat.Pretty);
			bool decoded = format.HasFlag(ToStringFormat.Decoded);
			int indent = 0;
			this.CalculateStringSize(false, in pretty, in decoded, ref count, ref indent);

			char[] buffer = ArrayPool<char>.Shared.Rent(count);
			indent = 0;
			int pos = 0;
			this.ProcessString(false, in pretty, in decoded, ref indent, in buffer, ref pos);

			string builtString = new string(buffer.AsSpan()[..count]);
			ArrayPool<char>.Shared.Return(buffer);
			return builtString;
		}

		private readonly void CalculateStringSize(bool AsValue, in bool pretty, in bool decoded, ref int count, ref int indent) {
			switch (this.Type) {
				case JsonType.String:
					count += (decoded ? this.GetStringDecodeLength() : this.ReferenceData.Length) + 2;
					break;
				case JsonType.Null:
				case JsonType.Number:
				case JsonType.Boolean:
					count += this.ReferenceData.Length;
					break;
				case JsonType.Object: {
					int innerCount = this.InnerValues.Length;
					if (pretty && !AsValue) {
						count += indent * INDENT_LEN;
					}
					count++;

					if (innerCount == 0) {
						if (pretty) {
							count += INDENT_LEN;
						}
						count++;
						break;
					}
					indent++;
					if (pretty) {
						count += innerCount * ((indent * INDENT_LEN) + 6);
						for (int x = 0; x < innerCount; x++) {
							NJson value = this.InnerValues[x];
							count += value.KeyData.Length;
							value.CalculateStringSize(true, in pretty, in decoded, ref count, ref indent);
						}
					}
					else {
						count += (innerCount * 5) - 1;
						for (int x = 0; x < innerCount; x++) {
							NJson value = this.InnerValues[x];
							count += value.KeyData.Length;
							value.CalculateStringSize(true, in pretty, in decoded, ref count, ref indent);
						}
					}

					indent--;
					if (pretty) {
						count += indent * INDENT_LEN;
					}
					count++;
					break;
				}
				case JsonType.Array: {
					int innerCount = this.InnerValues.Length;
					if (pretty && !AsValue) {
						count += indent * INDENT_LEN;
					}
					count++;

					if (innerCount == 0) {
						if (pretty) {
							count += INDENT_LEN;
						}
						count++;
					}
					else {
						indent++;
						if (pretty) {
							count += innerCount * 2;
							for (int x = 0; x < innerCount; x++) {
								NJson value = this.InnerValues[x];
								switch (value.Type) {
									case JsonType.Null:
									case JsonType.String:
									case JsonType.Number:
									case JsonType.Boolean:
										count += indent * INDENT_LEN;
										break;
								}
								value.CalculateStringSize(false, in pretty, in decoded, ref count, ref indent);
							}
						}
						else {
							count += innerCount - 1;
							for (int x = 0; x < innerCount; x++) {
								NJson value = this.InnerValues[x];
								value.CalculateStringSize(false, in pretty, in decoded, ref count, ref indent);
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
		}

		/// <summary>
		/// Recursive method to build the json ToString output
		/// </summary>
		/// <param name="AsValue"></param>
		/// <param name="pretty"></param>
		/// <param name="indent"></param>
		/// <param name="sb"></param>
		private readonly void ProcessString(bool AsValue, in bool pretty, in bool decoded, ref int indent, in char[] sb, ref int sbPos) {
			switch (this.Type) {
				case JsonType.String: {
					sb[sbPos++] = '"';
					if (decoded) {
						char[] buffer = ArrayPool<char>.Shared.Rent(this.ReferenceData.Length);
						this.RentStringDecodedIntoBuffer(in buffer, out int len);
						for (int x = 0; x < len; x++) {
							sb[sbPos++] = buffer[x];
						}
						ArrayPool<char>.Shared.Return(buffer);
					}
					else {
						ReadOnlySpan<char> refSpan = this.ReferenceData.Span;
						int redLen = refSpan.Length;
						for (int x = 0; x < redLen; x++) {
							sb[sbPos++] = refSpan[x];
						}
					}
					sb[sbPos++] = '"';
					break;
				}
				case JsonType.Null:
				case JsonType.Number:
				case JsonType.Boolean: {
					ReadOnlySpan<char> refSpan = this.ReferenceData.Span;
					for (int x = 0; x < this.ReferenceData.Length; x++) {
						sb[sbPos++] = refSpan[x];
					}
					break;
				}
				case JsonType.Object: {
					int innerCount = this.InnerValues.Length;
					ReadOnlySpan<char> indentSpan = pretty ? INDENT_TABS.AsSpan() : ReadOnlySpan<char>.Empty;
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							for (int y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
					}
					sb[sbPos++] = '{';

					if (innerCount == 0) {
						if (pretty) {
							for (int y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
						sb[sbPos++] = '}';
						break;
					}
					indent++;
					int limit = innerCount - 1;
					ReadOnlySpan<char> keySpan;
					NJson value;
					int keyLen;
					if (pretty) {
						sb[sbPos++] = '\n';
						int x = 0;
						for (; x < limit; x++) {
							value = this.InnerValues[x];
							for (int y = 0; y < indent; y++) {
								for (int z = 0; z < INDENT_LEN; z++) {
									sb[sbPos++] = indentSpan[z];
								}
							}
							sb[sbPos++] = '"';
							keySpan = value.KeyData.Span;
							keyLen = keySpan.Length;
							for (int y = 0; y < keyLen; y++) {
								sb[sbPos++] = keySpan[y];
							}
							sb[sbPos++] = '"';
							sb[sbPos++] = ':';
							sb[sbPos++] = ' ';
							value.ProcessString(true, in pretty, in decoded, ref indent, in sb, ref sbPos);
							sb[sbPos++] = ',';
							sb[sbPos++] = '\n';
						}
						value = this.InnerValues[x];
						for (int y = 0; y < indent; y++) {
							for (int z = 0; z < INDENT_LEN; z++) {
								sb[sbPos++] = indentSpan[z];
							}
						}
						sb[sbPos++] = '"';
						keySpan = value.KeyData.Span;
						keyLen = keySpan.Length;
						for (int y = 0; y < keyLen; y++) {
							sb[sbPos++] = keySpan[y];
						}
						sb[sbPos++] = '"';
						sb[sbPos++] = ':';
						sb[sbPos++] = ' ';
						value.ProcessString(true, in pretty, in decoded, ref indent, in sb, ref sbPos);
						sb[sbPos++] = '\n';
					}
					else {
						int x = 0;
						for (; x < limit; x++) {
							value = this.InnerValues[x];
							sb[sbPos++] = '"';
							keySpan = value.KeyData.Span;
							keyLen = keySpan.Length;
							for (int y = 0; y < keyLen; y++) {
								sb[sbPos++] = keySpan[y];
							}
							sb[sbPos++] = '"';
							sb[sbPos++] = ':';
							sb[sbPos++] = ' ';
							value.ProcessString(true, in pretty, in decoded, ref indent, in sb, ref sbPos);
							sb[sbPos++] = ',';
						}
						value = this.InnerValues[x];
						sb[sbPos++] = '"';
						keySpan = value.KeyData.Span;
						keyLen = keySpan.Length;
						for (int y = 0; y < keyLen; y++) {
							sb[sbPos++] = keySpan[y];
						}
						sb[sbPos++] = '"';
						sb[sbPos++] = ':';
						sb[sbPos++] = ' ';
						value.ProcessString(true, in pretty, in decoded, ref indent, in sb, ref sbPos);
					}

					indent--;
					if (pretty) {
						for (int x = 0; x < indent; x++) {
							for (int y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
					}
					sb[sbPos++] = '}';
					break;
				}
				case JsonType.Array: {
					int innerCount = this.InnerValues.Length;
					ReadOnlySpan<char> indentSpan = pretty ? INDENT_TABS.AsSpan() : ReadOnlySpan<char>.Empty;
					if (pretty && !AsValue) {
						for (int x = 0; x < indent; x++) {
							for (int y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
					}
					sb[sbPos++] = '[';

					if (innerCount == 0) {
						if (pretty) {
							for (int y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
						sb[sbPos++] = ']';
					}
					else {
						indent++;
						int limit = innerCount - 1;
						NJson value;
						if (pretty) {
							sb[sbPos++] = '\n';
							int x = 0;
							for (; x < limit; x++) {
								value = this.InnerValues[x];
								switch (value.Type) {
									case JsonType.Null:
									case JsonType.String:
									case JsonType.Number:
									case JsonType.Boolean:
										for (int y = 0; y < indent; y++) {
											for (int z = 0; z < INDENT_LEN; z++) {
												sb[sbPos++] = indentSpan[z];
											}
										}
										break;
								}
								value.ProcessString(false, in pretty, in decoded, ref indent, in sb, ref sbPos);
								sb[sbPos++] = ',';
								sb[sbPos++] = '\n';
							}
							value = this.InnerValues[x];
							switch (value.Type) {
								case JsonType.Null:
								case JsonType.String:
								case JsonType.Number:
								case JsonType.Boolean:
									for (int y = 0; y < indent; y++) {
										for (int z = 0; z < INDENT_LEN; z++) {
											sb[sbPos++] = indentSpan[z];
										}
									}
									break;
							}
							value.ProcessString(false, in pretty, in decoded, ref indent, in sb, ref sbPos);
							sb[sbPos++] = '\n';
						}
						else {
							int x = 0;
							for (; x < limit; x++) {
								value = this.InnerValues[x];
								value.ProcessString(false, in pretty, in decoded, ref indent, in sb, ref sbPos);
								sb[sbPos++] = ',';
							}
							value = this.InnerValues[x];
							value.ProcessString(false, in pretty, in decoded, ref indent, in sb, ref sbPos);
						}

						indent--;
						if (pretty) {
							for (int x = 0; x < indent; x++) {
								for (int y = 0; y < INDENT_LEN; y++) {
									sb[sbPos++] = indentSpan[y];
								}
							}
						}
						sb[sbPos++] = ']';
					}
					break;
				}
			}
		}

		/// <summary>
		/// Get the literal string value of the object
		/// </summary>
		public string GetStringLiteral => this.ReferenceData.ToString();

		/// <param name="buffer">Designed to take in an array provided by ArrayBuffer</param> 
		private void RentStringDecodedIntoBuffer(in char[] buffer, out int newLen) {
			int x = 0;
			int len = this.ReferenceData.Length;
			newLen = 0;
			ReadOnlySpan<char> data = this.ReferenceData.Span;
			char c;
			while (x < len) {
				c = data[x];
				switch (c) {
					case '\\':
						c = data[++x];
						switch (c) {
							case '\\':
							case '/':
							case '"':
								buffer[newLen++] = c;
								break;
							case 'f':
								buffer[newLen++] = '\f';
								break;
							case 'b':
								buffer[newLen++] = '\b';
								break;
							case 'n':
								buffer[newLen++] = '\n';
								break;
							case 'r':
								buffer[newLen++] = '\r';
								break;
							case 't':
								buffer[newLen++] = '\t';
								break;
							case 'u': {
								buffer[newLen++] = (char)((ReadHexNumber(data[++x]) * 4096)
										+ (ReadHexNumber(data[++x]) * 256)
										+ (ReadHexNumber(data[++x]) * 16)
										+ ReadHexNumber(data[++x]));
								break;
							}
						}
						break;
					default:
						buffer[newLen++] = c;
						break;
				}
				x++;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		private int GetStringDecodeLength() {
			int count = 0;
			int x = 0;
			int len = this.ReferenceData.Length;
			ReadOnlySpan<char> data = this.ReferenceData.Span;
			char c;
			while (x < len) {
				c = data[x];
				switch (c) {
					case '\\':
						c = data[++x];
						if (c == 'u') {
							x += 4;
						}
						count++;
						break;
					default:
						count++;
						break;
				}
				x++;
			}
			return count;
		}

		/// <summary>
		/// Get the decoded string value of the object
		/// </summary>
		public string GetStringDecoded
		{
			get
			{
				int x = 0;
				int len = this.ReferenceData.Length;
				ReadOnlySpan<char> data = this.ReferenceData.Span;
				char c;

				char[] buffer = ArrayPool<char>.Shared.Rent(len);
				x = 0;
				int y = 0;
				while (x < len) {
					c = data[x];
					switch (c) {
						case '\\':
							c = data[++x];
							switch (c) {
								case '\\':
								case '/':
								case '"':
									buffer[y++] = c;
									break;
								case 'f':
									buffer[y++] = '\f';
									break;
								case 'b':
									buffer[y++] = '\b';
									break;
								case 'n':
									buffer[y++] = '\n';
									break;
								case 'r':
									buffer[y++] = '\r';
									break;
								case 't':
									buffer[y++] = '\t';
									break;
								case 'u': {
									buffer[y++] = (char)((ReadHexNumber(data[++x]) * 4096)
										+ (ReadHexNumber(data[++x]) * 256)
										+ (ReadHexNumber(data[++x]) * 16)
										+ (ReadHexNumber(data[++x])));
									break;
								}
							}
							break;
						default:
							buffer[y++] = c;
							break;
					}
					x++;
				}
				string decodedValue = buffer.AsSpan()[..y].ToString();
				ArrayPool<char>.Shared.Return(buffer);
				return decodedValue;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public static int ReadHexNumber(char character) {
			return character switch {
				'0' => 0,
				'1' => 1,
				'2' => 2,
				'3' => 3,
				'4' => 4,
				'5' => 5,
				'6' => 6,
				'7' => 7,
				'8' => 8,
				'9' => 9,
				'A' => 10,
				'B' => 11,
				'C' => 12,
				'D' => 13,
				'E' => 14,
				'F' => 15,
				_ => throw new InvalidOperationException($"{nameof(character)} '{character}' is not a hex number")
			};
		}

		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public string TryGetString(string key, bool decoded = true) => this.TryGetString(key.AsSpan(), decoded);
		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetString(string key, out string @out, bool decoded = true) => this.TryGetString(key.AsSpan(), out @out, decoded);

		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public string TryGetString(ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(key, out NJson value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetString(ReadOnlySpan<char> key, out string @out, bool decoded = true) {
			if (this.TryGetKey(key, out NJson value) && value.Type == JsonType.String) {
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
		public readonly ReadOnlySpan<char> GetSpan => this.ReferenceData.Span;

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public readonly double GetNumber => double.TryParse(this.ReferenceData.Span, out double value) ? value : double.NaN;

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public readonly T GetNumberOfType<T>() where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			return double.TryParse(this.ReferenceData.Span, out double value) ? Number.GetValue<T>(value) : Number.GetEmpty<T>();
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
		public bool TryGetNumber(string key, out double @out) => this.TryGetNumber(@key.AsSpan(), out @out);

		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public double TryGetNumber(ReadOnlySpan<char> key) => this.TryGetKey(key, out NJson value) ? value.GetNumber : double.NaN;
		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetNumber(ReadOnlySpan<char> key, out double @out) {
			if (this.TryGetKey(key, out NJson value) && value.Type == JsonType.Number) {
				return double.TryParse(this.ReferenceData.Span, out @out);
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
		public T TryGetNumber<T>(ReadOnlySpan<char> key) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			this.TryGetNumber(key, out T value);
			return value;
		}

		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetNumber<T>(ReadOnlySpan<char> key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			if (this.TryGetKey(key, out NJson value) && value.Type == JsonType.Number) {
				@out = value.GetNumberOfType<T>();
				return true;
			}
			else {
				@out = Number.GetEmpty<T>();
				return false;
			}
		}


		/// <summary>
		/// Get the values contained inside This object but as a new array
		/// </summary>
		public NJson[] GetCopyOfInsideValues => this.InnerValues.Clone();

		/// <summary>
		/// Get the values contained inside This object
		/// </summary>
		public ReadOnlySpan<NJson> GetInsideValues => this.InnerValues.GetSpan;

		/// <summary>
		/// Gets the length of the contained values for Array or Object
		/// </summary>
		public int InnerLength => this.InnerValues.Length;

		/// <summary>
		/// Get if This object is Null
		/// </summary>
		public readonly bool IsNull => this.Type == JsonType.Null;

		/// <summary>
		/// Get the bool value of This object
		/// </summary>
		public readonly bool GetBool => bool.TryParse(this.ReferenceData.Span, out bool value) && value;

		/// <summary>
		/// Try to get the bool value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetBool(string key) => this.GetKeyOrEmpty(key.AsSpan()).GetBool;

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
		public bool TryGetBool(ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(key, out NJson value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

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
			return MemoryExtensions.SequenceEqual(key, this.KeyData.Span);
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key Found</returns>
		public readonly bool TryGetKey(string key, out NJson found) => this.TryGetKey(key.AsSpan(), out found);
		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key Found</returns>
		public readonly bool TryGetKey(ReadOnlySpan<char> key, out NJson found) {
			if (this.Type == JsonType.Object) {
				int pathLen = key.Length;
				int innerCount = this.InnerValues.Length;
				for (int x = 0; x < innerCount; x++) {
					NJson value = this.InnerValues[x];
					ReadOnlySpan<char> valueKey = value.KeyData.Span;
					int len = valueKey.Length;
					if (pathLen == len) {
						if (key.StartsWith(valueKey)) {
							found = value;
							return true;
						}
					}
					else if (pathLen > len) {
						if (key.StartsWith(valueKey) && value.TryGetKey(key[++len..], out found)) {
							return true;
						}
					}
				}
			}

			found = NJson.Empty;
			return false;
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		public readonly NJson GetKeyOrEmpty(string key) => this.GetKeyOrEmpty(key.AsSpan());
		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		public readonly NJson GetKeyOrEmpty(ReadOnlySpan<char> key) => this.TryGetKey(key, out NJson found) ? found : NJson.Empty;

		public readonly Enumerator GetEnumerator() => new Enumerator(this);

		public readonly override bool Equals(object obj) => obj is NJson other && this.Equals(other);
		public readonly bool Equals(NJson other) {
			if (this.Type.Equals(other.Type)
				&& this.InnerValues.Length.Equals(other.InnerValues.Length)
				&& this.CompareKey(other.KeyData.Span)
				&& MemoryExtensions.Equals(this.ReferenceData, other.ReferenceData)) {
				return true;
			}
			return false;
		}
		public static bool operator ==(NJson left, NJson right) {
			return left.Equals(right);
		}

		public static bool operator !=(NJson left, NJson right) {
			return !(left == right);
		}

		public readonly override int GetHashCode() {
			return HashCode.Combine(this.Type, this.InnerValues, this.ReferenceData, this.KeyData);
		}

		public static implicit operator NJson(nJson span) {
			return NJson.Pin(span);
		}
		public static implicit operator nJson(NJson self) {
			if (self.KeyData.IsEmpty) {
				return new nJson(self.ReferenceData.Span);
			}
			else {
				return new nJson(self.KeyData.Span, self.ReferenceData.Span);
			}
		}

		internal static class Number {
			public static T GetValue<T>(double value) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
				return (T)(object)(typeof(T).Name switch {
					"SByte" => Convert.ToSByte(value),
					"Byte" => Convert.ToByte(value),
					"Int16" => Convert.ToInt16(value),
					"UInt16" => Convert.ToUInt16(value),
					"Int32" => Convert.ToInt32(value),
					"UInt32" => Convert.ToUInt32(value),
					"Int64" => Convert.ToInt64(value),
					"UInt64" => Convert.ToUInt64(value),
					"Single" => Convert.ToSingle(value),
					"Double" => value,
					"Decimal" => Convert.ToDecimal(value),
					_ => throw new NotSupportedException(typeof(T).Name),
				});
			}

			public static T GetEmpty<T>() where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
				return (T)(object)(typeof(T).Name switch {
					"SByte" => (sbyte)0,
					"Byte" => (byte)0,
					"Int16" => (short)0,
					"UInt16" => (ushort)0,
					"Int32" => 0,
					"UInt32" => (uint)0,
					"Int64" => (long)0,
					"UInt64" => (ulong)0,
					"Single" => 0.0f,
					"Double" => 0.0d,
					"Decimal" => (decimal)0,
					_ => throw new NotSupportedException(typeof(T).Name),
				});
			}
		}
	}
}
