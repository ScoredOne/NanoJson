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

namespace NanoJson {

#pragma warning disable IDE1006 // Naming Styles
	public readonly ref struct nJson {
#pragma warning restore IDE1006 // Naming Styles
		public ref struct Enumerator {
			private readonly nJson owner;
			private readonly int len;

			private nJson current;

			private int index;
			private int x;
			private int y;
			private int debth;
			private int arrayPos;

			internal Enumerator(nJson owner) {
				this.owner = owner;
				this.len = owner.Value.Length;
				this.current = nJson.Empty;

				this.index = -1;
				this.x = 0;
				this.y = -1;
				this.debth = 0;
				this.arrayPos = 0;
			}

			public readonly nJson Current => this.current;

			public nJson this[int index]
			{
				get
				{
					if (this.TryGetIndex(index, out nJson value)) {
						return value;
					}
					throw new IndexOutOfRangeException();
				}
			}

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
								while (true) {
									if (this.owner.Value[++this.x] == '"') {
										break;
									}
								}
								name = this.owner.Value[this.y..this.x];
							}

							while (true) {
								if (this.owner.Value[++this.x] == ':') {
									break;
								}
							}
							while (NJson.IsWhiteSpace(this.owner.Value[++this.x])) { }
							this.y = this.x;
							this.index++;
							while (true) {
								switch (this.owner.Value[this.x]) {
									case '"':
										while (true) {
											if (this.owner.Value[++this.x] == '"') {
												break;
											}
										}
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
							while (true) {
								if (this.owner.Value[this.x++] == '[') {
									break;
								}
							}
						}
						this.y = this.x;
						this.debth = 1;

						while (true) {
							while (true) {
								switch (this.owner.Value[this.x]) {
									case '"':
										while (true) {
											if (this.owner.Value[++this.x] == '"') {
												break;
											}
										}
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
			this.Key = ReadOnlySpan<char>.Empty;
			if (data.IsWhiteSpace()) {
				this.Type = JsonType.Null;
				this.Value = ReadOnlySpan<char>.Empty;
				this.IsEmpty = true;
				return;
			}

			int x = 0;
			char c = data[0];
			while (NJson.IsWhiteSpace(c)) {
				c = data[++x];
			}
			switch (c) {
				case '"': {
					this.Type = JsonType.String;
					int first = ++x;
					x = data.Length;

					while (true) {
						if (data[--x] == '"') {
							break;
						}
					}

					if (x < first) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					else if (first == x) {
						this.Value = string.Empty.AsSpan();
						this.IsEmpty = true;
					}
					else {
						this.Value = data[first..x];
						this.IsEmpty = false;
					}
					return;
				}
				case '[': {
					this.Type = JsonType.Array;
					int first = x;
					int len = data.Length;

					while (true) {
						if (data[--len] == ']') {
							break;
						}
					}

					if (len <= x) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					while (NJson.IsWhiteSpace(data[++x])) { }
					if (x == len++) {
						this.Value = data[first..len];
						this.IsEmpty = true;
						return;
					}

					this.Value = data[first..len];
					this.IsEmpty = false;
					return;
				}
				case '{': {
					this.Type = JsonType.Object;
					int first = x;
					int len = data.Length;

					while (true) {
						if (data[--len] == '}') {
							break;
						}
					}

					if (len <= x) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					while (NJson.IsWhiteSpace(data[++x])) { }
					if (x == len++) {
						this.Value = data[first..len];
						this.IsEmpty = true;
						return;
					}

					this.Value = data[first..len];
					this.IsEmpty = false;
					return;
				}
				case 'n':
				case 'N': {
					this.Type = JsonType.Null;
					this.Value = NJson.NULL.AsSpan();
					this.IsEmpty = false;
					if (data.Length - x == 4) {
						c = data[++x];
						if (c == 'u' || c == 'U') {
							c = data[++x];
							if (c == 'l' || c == 'L') {
								c = data[++x];
								if (c == 'l' || c == 'L') {
									return;
								}
							}
						}
					}

					throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
				}
				case 't':
				case 'T': {
					this.Type = JsonType.Boolean;
					this.Value = bool.TrueString.AsSpan();
					this.IsEmpty = false;
					if (data.Length - x == 4) {
						c = data[++x];
						if (c == 'r' || c == 'R') {
							c = data[++x];
							if (c == 'u' || c == 'U') {
								c = data[++x];
								if (c == 'e' || c == 'E') {
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
					this.Value = bool.FalseString.AsSpan();
					this.IsEmpty = false;
					if (data.Length - x == 5) {
						c = data[++x];
						if (c == 'a' || c == 'A') {
							c = data[++x];
							if (c == 'l' || c == 'L') {
								c = data[++x];
								if (c == 's' || c == 'S') {
									c = data[++x];
									if (c == 'e' || c == 'E') {
										return;
									}
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
					this.Value = data.Slice(x);
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
								|| !((c = this.Value[++x]) == '+' || c == '-')
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
						if (this.TryGetKey(key, out nJson v)) {
							return v;
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
				if (this.IsEmpty) {
					throw new IndexOutOfRangeException("Body is Empty");
				}
				if (this.GetEnumerator().TryGetIndex(index, out nJson v)) {
					return v;
				}
				throw new IndexOutOfRangeException();
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
			int y;
			int debth = 0;

			bool found = false;

			while (true) {
				while (true) {
					if (this.Value[x] == '"') {
						break;
					}
					x++;
				}
				ReadOnlySpan<char> name;
				if (this.Value[++x] == '"') {
					name = ReadOnlySpan<char>.Empty;
				}
				else {
					y = x;
					while (true) {
						if (this.Value[++x] == '"') {
							break;
						}
					}
					name = this.Value[y..x];
				}

				if (key.StartsWith(name)) {
					nameLen = name.Length;
					found = true;
				}

				while (true) {
					if (this.Value[++x] == ':') {
						break;
					}
				}
				while (NJson.IsWhiteSpace(this.Value[++x])) { }
				y = x;
				while (true) {
					switch (this.Value[x]) {
						case '"':
							while (true) {
								if (this.Value[++x] == '"') {
									break;
								}
							}
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
				string decodedValue = new string(buffer.AsSpan(0, y));
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
			return double.TryParse(this.Value, out double value) ? NJson.GetConvertedValue<T>(value) : default;
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

			public readonly NJson[] Clone() => this.Length == 0 ? Array.Empty<NJson>() : (NJson[])this.Data.Clone();

			public readonly ReadOnlySpan<NJson> GetSpan => this.Data.AsSpan();

			public ref struct Enumerator {
				private readonly NanoArray owner;
				private int index;

				public readonly ref NJson Current => ref this.owner[this.index];

				public Enumerator(NanoArray owner) {
					this.owner = owner;
					this.index = -1;
				}

				public bool MoveNext() => ++this.index < this.owner.Length;

				public void Reset() => this.index = -1;
			}
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
				return new NJson(key, data, -1);
			}
		}

		public static NJson ParseJson(string data) {
			return NJson.ParseJson(data.AsMemory());
		}

		private static NJson ParseJson(ReadOnlyMemory<char> data) {
			return new NJson(ReadOnlyMemory<char>.Empty, data, -1);
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
					}
					else {
						return new NJson(data.KeyData.ToArray());
					}
				case JsonType.Object:
				case JsonType.Array: // Inner bodies need re-parsing as the originals reference the same allocated memory and we want it to point to a new area
					return new NJson(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray());
				case JsonType.String:
					return NJson.CreateStringObject(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray());
				case JsonType.Number:
					return NJson.CreateNumberObject(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, double.Parse(data.ReferenceData.Span));
				case JsonType.Boolean:
					return NJson.CreateBoolObject(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, bool.Parse(data.ReferenceData.Span));
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
			return NJson.CreateObject(key.AsMemory(), data);
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
			return new NJson(key, data);
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

		public readonly static NJson Empty = new NJson(ReadOnlyMemory<char>.Empty, ReadOnlyMemory<char>.Empty, -1, -1);

		public readonly JsonType Type;
		private readonly NanoArray InnerValues;
		private readonly ReadOnlyMemory<char> ReferenceData;
		private readonly ReadOnlyMemory<char> KeyData;

		private NJson(ReadOnlyMemory<char> key, ReadOnlyMemory<char> reference, int innerLength = -1, int knownLen = -1) {
			this.KeyData = key;
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
			while (IsWhiteSpace(c)) {
				c = data[++x];
			}
			this.ReferenceData = reference[x..];
			switch (c) {
				case '"': {
					this.Type = JsonType.String;
					this.InnerValues = NanoArray.Empty;
					int first = ++x;

					while (true) {
						if (data[--len] == '"') {
							break;
						}
					}

					if (len < first) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
					}
					else if (first == len) {
						this.ReferenceData = string.Empty.AsMemory();
						return;
					}
					else {
						this.ReferenceData = reference[first..len];
						return;
					}
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
					while (IsWhiteSpace(data[++x])) { }
					if (x == len) {
						this.InnerValues = NanoArray.Empty;
						return;
					}

					if (innerLength == -1) {
						int debth = 0;
						x = first;

						while (true) {
							switch (data[x]) {
								case '"':
									while (true) {
										if (data[++x] == '"') {
											break;
										}
									}
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
										innerLength++;
									}
									break;
							}
							x++;
						}
					}
					Fin:
					this.InnerValues = new NanoArray(innerLength);

					this.ProcessJsonArray(reference, first, innerLength);
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
					while (IsWhiteSpace(data[++x])) { }
					if (x == len) {
						this.InnerValues = NanoArray.Empty;
						return;
					}

					if (innerLength == -1) {
						int debth = 0;
						x = first;
						while (true) {
							switch (data[x]) {
								case '"':
									while (true) {
										if (data[++x] == '"') {
											break;
										}
									}
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
										innerLength++;
									}
									break;
							}
							x++;
						}
					}
					Fin:
					this.InnerValues = new NanoArray(innerLength);

					this.ProcessJsonObject(reference, first, innerLength);
					return;
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
						}
						else if (c == 'e' || c == 'E') {
							if (E
								|| !((c = data[++x]) == '+' || c == '-')
								|| !char.IsDigit(data[++x])) {
								throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
							}
							E = true;
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

		private NJson(JsonType type, params NJson[] contents) : this(ReadOnlyMemory<char>.Empty, type, contents) { }

		private NJson(ReadOnlyMemory<char> key, JsonType type, params NJson[] contents) {
			switch (type) {
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

		private NJson(ReadOnlyMemory<char> key, NJson value) {
			this.KeyData = key;
			this.Type = value.Type;
			this.ReferenceData = value.ReferenceData;
			this.InnerValues = value.InnerValues;
		}

		private NJson(ReadOnlyMemory<char> key, bool value) {
			this.KeyData = key;
			this.Type = JsonType.Boolean;
			this.ReferenceData = value ? bool.TrueString.AsMemory() : bool.FalseString.AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(ReadOnlyMemory<char> key, double value) {
			this.KeyData = key;
			this.Type = JsonType.Number;
			this.ReferenceData = value.ToString().AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(ReadOnlyMemory<char> key) {
			this.KeyData = key;
			this.Type = JsonType.Null;
			this.ReferenceData = NULL.AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(ReadOnlyMemory<char> key, ReadOnlyMemory<char> value) {
			this.KeyData = key;
			this.Type = JsonType.String;
			this.ReferenceData = value;
			this.InnerValues = NanoArray.Empty;
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key">Key or Path to desired value</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException">Key was not found in object</exception>
		/// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
		public readonly NJson this[string path] => this[path.AsSpan()];

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key">Key or Path to desired value</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException">Key was not found in object</exception>
		/// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
		public readonly NJson this[ReadOnlySpan<char> key]
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
		public readonly NJson this[int index]
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
		private readonly void ProcessJsonArray(ReadOnlyMemory<char> reference, int start, int innerCount) {
			ReadOnlySpan<char> data = reference.Span;
			int x = start;
			while (true) {
				if (data[x++] == '[') {
					break;
				}
			}
			int y = x;
			int debth = 1;
			int index = 0;
			int innerSize;

			while (true) {
				innerSize = 1;
				while (true) {
					switch (data[x]) {
						case '"':
							while (true) {
								if (data[++x] == '"') {
									break;
								}
							}
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
				this.InnerValues[index++] = new NJson(ReadOnlyMemory<char>.Empty, reference[y..x], innerSize, x - y);
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
		private readonly void ProcessJsonObject(ReadOnlyMemory<char> reference, int start, int innerCount) {
			ReadOnlySpan<char> data = reference.Span;

			int x = start;
			int y;
			int debth = 0;
			int index = 0;
			int innerSize;

			while (true) {
				while (true) {
					if (data[x] == '"') {
						break;
					}
					x++;
				}
				ReadOnlyMemory<char> name;
				if (data[++x] == '"') {
					name = ReadOnlyMemory<char>.Empty;
				}
				else {
					y = x;
					while (true) {
						if (data[++x] == '"') {
							break;
						}
					}
					name = reference[y..x];
				}

				while (true) {
					if (data[++x] == ':') {
						break;
					}
				}
				while (IsWhiteSpace(data[++x])) { }
				y = x;
				innerSize = 1;
				while (true) {
					switch (data[x]) {
						case '"':
							while (true) {
								if (data[++x] == '"') {
									break;
								}
							}
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
				this.InnerValues[index++] = new NJson(name, reference[y..x], innerSize, x - y);
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

			string builtString = new string(buffer.AsSpan(0, count));

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
					int x;

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
						for (x = 0; x < innerCount; x++) {
							NJson value = this.InnerValues[x];
							count += value.KeyData.Length;
							value.CalculateStringSize(true, in pretty, in decoded, ref count, ref indent);
						}
					}
					else {
						count += (innerCount * 5) - 1;
						for (x = 0; x < innerCount; x++) {
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
					int x;

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
							for (x = 0; x < innerCount; x++) {
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
							for (x = 0; x < innerCount; x++) {
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
					int x;
					int y;
					if (pretty && !AsValue) {
						for (x = 0; x < indent; x++) {
							for (y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
					}
					sb[sbPos++] = '{';

					if (innerCount == 0) {
						if (pretty) {
							for (y = 0; y < INDENT_LEN; y++) {
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
						for (x = 0; x < limit; x++) {
							value = this.InnerValues[x];
							for (y = 0; y < indent; y++) {
								for (int z = 0; z < INDENT_LEN; z++) {
									sb[sbPos++] = indentSpan[z];
								}
							}
							sb[sbPos++] = '"';
							keySpan = value.KeyData.Span;
							keyLen = keySpan.Length;
							for (y = 0; y < keyLen; y++) {
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
						for (y = 0; y < indent; y++) {
							for (int z = 0; z < INDENT_LEN; z++) {
								sb[sbPos++] = indentSpan[z];
							}
						}
						sb[sbPos++] = '"';
						keySpan = value.KeyData.Span;
						keyLen = keySpan.Length;
						for (y = 0; y < keyLen; y++) {
							sb[sbPos++] = keySpan[y];
						}
						sb[sbPos++] = '"';
						sb[sbPos++] = ':';
						sb[sbPos++] = ' ';
						value.ProcessString(true, in pretty, in decoded, ref indent, in sb, ref sbPos);
						sb[sbPos++] = '\n';

						indent--;
						for (x = 0; x < indent; x++) {
							for (y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
					}
					else {
						for (x = 0; x < limit; x++) {
							value = this.InnerValues[x];
							sb[sbPos++] = '"';
							keySpan = value.KeyData.Span;
							keyLen = keySpan.Length;
							for (y = 0; y < keyLen; y++) {
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
						for (y = 0; y < keyLen; y++) {
							sb[sbPos++] = keySpan[y];
						}
						sb[sbPos++] = '"';
						sb[sbPos++] = ':';
						sb[sbPos++] = ' ';
						value.ProcessString(true, in pretty, in decoded, ref indent, in sb, ref sbPos);
					}

					sb[sbPos++] = '}';
					break;
				}
				case JsonType.Array: {
					int innerCount = this.InnerValues.Length;
					ReadOnlySpan<char> indentSpan = pretty ? INDENT_TABS.AsSpan() : ReadOnlySpan<char>.Empty;
					int x;
					int y;
					if (pretty && !AsValue) {
						for (x = 0; x < indent; x++) {
							for (y = 0; y < INDENT_LEN; y++) {
								sb[sbPos++] = indentSpan[y];
							}
						}
					}
					sb[sbPos++] = '[';

					if (innerCount == 0) {
						if (pretty) {
							for (y = 0; y < INDENT_LEN; y++) {
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
							for (x = 0; x < limit; x++) {
								value = this.InnerValues[x];
								switch (value.Type) {
									case JsonType.Null:
									case JsonType.String:
									case JsonType.Number:
									case JsonType.Boolean:
										for (y = 0; y < indent; y++) {
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
									for (y = 0; y < indent; y++) {
										for (int z = 0; z < INDENT_LEN; z++) {
											sb[sbPos++] = indentSpan[z];
										}
									}
									break;
							}
							value.ProcessString(false, in pretty, in decoded, ref indent, in sb, ref sbPos);
							sb[sbPos++] = '\n';

							indent--;
							for (x = 0; x < indent; x++) {
								for (y = 0; y < INDENT_LEN; y++) {
									sb[sbPos++] = indentSpan[y];
								}
							}
						}
						else {
							for (x = 0; x < limit; x++) {
								value = this.InnerValues[x];
								value.ProcessString(false, in pretty, in decoded, ref indent, in sb, ref sbPos);
								sb[sbPos++] = ',';
							}
							value = this.InnerValues[x];
							value.ProcessString(false, in pretty, in decoded, ref indent, in sb, ref sbPos);
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
		public readonly string GetStringLiteral => this.ReferenceData.ToString();

		/// <param name="buffer">Designed to take in an array provided by ArrayBuffer</param>
		private readonly void RentStringDecodedIntoBuffer(in char[] buffer, out int newLen) {
			int x = 0;
			int len = this.ReferenceData.Length;
			newLen = 0;
			ReadOnlySpan<char> data = this.ReferenceData.Span;
			char c;
			while (x < len) {
				c = data[x];
				if (c == '"') {
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
				}
				else {
					buffer[newLen++] = c;
				}
				x++;
			}
		}

		private int GetStringDecodeLength() {
			int count = 0;
			int x = 0;
			int len = this.ReferenceData.Length;
			ReadOnlySpan<char> data = this.ReferenceData.Span;
			while (x < len) {
				if (data[x] == '\\' && data[++x] == 'u') {
					x += 4;
				}
				count++;
				x++;
			}
			return count;
		}

		/// <summary>
		/// Get the decoded string value of the object
		/// </summary>
		public readonly string GetStringDecoded
		{
			get
			{
				int len = this.ReferenceData.Length;
				ReadOnlySpan<char> data = this.ReferenceData.Span;
				char c;

				char[] buffer = ArrayPool<char>.Shared.Rent(len);
				int x = 0;
				int y = 0;
				while (x < len) {
					c = data[x];
					if (c == '\\') {
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
					}
					else {
						buffer[y++] = c;
					}
					x++;
				}
				string decodedValue = new string(buffer.AsSpan(0, y));
				ArrayPool<char>.Shared.Return(buffer);
				return decodedValue;
			}
		}

		public static int ReadHexNumber(char character) {
			if (character < 'A') {
				return character - '0';
			}
			else {
				return 10 + (character - 'A');
			}
		}

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
		public readonly string TryGetString(ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(key, out NJson value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public readonly bool TryGetString(ReadOnlySpan<char> key, out string @out, bool decoded = true) {
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
		public readonly string GetValue => this.ReferenceData.ToString();

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
		public readonly double GetNumber => double.TryParse(this.ReferenceData.Span, out double value) ? value : double.NaN;

		/// <summary>
		/// Get the number contained inside This object
		/// </summary>
		public readonly T GetNumberOfType<T>() where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			return double.TryParse(this.ReferenceData.Span, out double value) ? GetConvertedValue<T>(value) : default;
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
		public readonly double TryGetNumber(ReadOnlySpan<char> key) => this.TryGetKey(key, out NJson value) ? value.GetNumber : double.NaN;
		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public readonly bool TryGetNumber(ReadOnlySpan<char> key, out double @out) {
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
		public readonly T TryGetNumber<T>(ReadOnlySpan<char> key) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			this.TryGetNumber(key, out T value);
			return value;
		}

		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public readonly bool TryGetNumber<T>(ReadOnlySpan<char> key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			if (this.TryGetKey(key, out NJson value) && value.Type == JsonType.Number) {
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
		public readonly NJson[] GetCopyOfInsideValues => this.InnerValues.Clone();

		/// <summary>
		/// Get the values contained inside This object
		/// </summary>
		public readonly ReadOnlySpan<NJson> GetInsideValues => this.InnerValues.GetSpan;

		/// <summary>
		/// Gets the length of the contained values for Array or Object
		/// </summary>
		public readonly int InnerLength => this.InnerValues.Length;

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
		public readonly bool TryGetBool(ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(key, out NJson value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

		/// <summary>
		/// Get the key of This object
		/// </summary>
		public readonly string GetKey
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
		public readonly ReadOnlySpan<char> GetKeyAsSpan
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
		/// Get the key of This object
		/// </summary>
		public readonly ReadOnlyMemory<char> GetKeyAsMemory
		{
			get
			{
				if (this.KeyData.IsEmpty) {
					return ReadOnlyMemory<char>.Empty;
				}
				else {
					return this.KeyData;
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
			return key.Equals(this.KeyData.Span, StringComparison.Ordinal);
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
					ref NJson value = ref this.InnerValues[x];
					ReadOnlySpan<char> valueKey = value.KeyData.Span;
					int len = valueKey.Length;
					if (key.StartsWith(valueKey)) {
						if (pathLen > len) {
							if (value.TryGetKey(key[++len..], out found)) {
								return true;
							}
						}
						else {
							found = value;
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

		public readonly NanoArray.Enumerator GetEnumerator() => this.InnerValues.GetEnumerator();

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

		internal static bool IsWhiteSpace(in char character) {
			switch (character) {
				case ' ':
				case '\t':
				case '\n':
				case '\r':
				case '\v':
				case '\f':
					return true;
				default:
					return false;
			}
		}

		internal static T GetConvertedValue<T>(double value) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			switch (typeof(T).Name) {
				case "SByte":
					if (Convert.ToSByte(value) is T sb) {
						return sb;
					}
					goto default;
				case "Byte":
					if (Convert.ToByte(value) is T b) {
						return b;
					}
					goto default;
				case "Int16":
					if (Convert.ToInt16(value) is T s) {
						return s;
					}
					goto default;
				case "UInt16":
					if (Convert.ToUInt16(value) is T us) {
						return us;
					}
					goto default;
				case "Int32":
					if (Convert.ToInt32(value) is T i) {
						return i;
					}
					goto default;
				case "UInt32":
					if (Convert.ToUInt32(value) is T ui) {
						return ui;
					}
					goto default;
				case "Int64":
					if (Convert.ToInt64(value) is T l) {
						return l;
					}
					goto default;
				case "UInt64":
					if (Convert.ToUInt64(value) is T ul) {
						return ul;
					}
					goto default;
				case "Single":
					if (Convert.ToSingle(value) is T f) {
						return f;
					}
					goto default;
				case "Double":
					if (value is T d) {
						return d;
					}
					goto default;
				case "Decimal":
					if (Convert.ToDecimal(value) is T de) {
						return de;
					}
					goto default;
				default:
					throw new NotSupportedException(typeof(T).Name);
			}
		}
	}
}
