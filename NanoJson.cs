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
			private int depth;
			private int arrayPos;

			internal Enumerator(in nJson owner) {
				this.owner = owner;
				this.len = owner.Value.Length;
				this.current = nJson.Empty;

				this.index = -1;
				this.x = 0;
				this.y = -1;
				this.depth = 0;
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
										this.depth++;
										break;
									case ']':
										this.depth--;
										break;
									case '}':
										if (--this.depth < 0) { // no comma found, process last segment
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (this.depth == 0) {
											goto ProcessJsonObject;
										}
										break;
								}
								this.x++;
							}

							ProcessJsonObject:
							int continuationPoint = this.x;
							while (NJson.IsWhiteSpace(this.owner.Value[--this.x])) { }
							this.x++;
							if (!name.IsEmpty) {
								if (this.y > this.x) {
									this.x = this.y;
								}
								this.current = new nJson(name, this.owner.Value[this.y..this.x]);
								this.x = continuationPoint;
								this.y = this.x;
								return true;
							}
							else if ((this.x = continuationPoint + 1) >= this.len) {
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
						this.depth = 1;

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
										this.depth++;
										break;
									case '}':
										this.depth--;
										break;
									case ']':
										if (--this.depth == 0) {
											goto ProcessJsonObject;
										}
										break;
									case ',':
										if (this.depth == 1) {
											goto ProcessJsonObject;
										}
										break;
								}
								this.x++;
							}

							ProcessJsonObject:
							int continuationPoint = this.x;
							while (NJson.IsWhiteSpace(this.owner.Value[this.x - 1])) {
								this.x--;
							}
							if (this.arrayPos == this.index) {
								if (this.y > this.x) {
									this.x = this.y;
								}
								this.current = new nJson(this.owner.Value[this.y..this.x]);
								this.x = continuationPoint;
								return true;
							}
							else {
								this.x = continuationPoint;
								if (++this.x >= this.len) {
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
				this.depth = 0;
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
		public nJson(in Span<char> key, in Span<char> value) : this((ReadOnlySpan<char>)key, (ReadOnlySpan<char>)value) { }
		public nJson(in ReadOnlySpan<char> key, in ReadOnlySpan<char> value) : this(value) {
			this.Key = key;
		}

		public nJson(string data) : this(data.AsSpan()) { }
		public nJson(in Span<char> data) : this((ReadOnlySpan<char>)data) { }
		public nJson(in ReadOnlySpan<char> data) {
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

					int z = x - first;
					if (z < 0) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					else if (z == 0) {
						this.Value = string.Empty.AsSpan();
						this.IsEmpty = true;
					}
					else {
						this.Value = data.Slice(first, z);
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
						this.Value = data.Slice(first, len - first);
						this.IsEmpty = true;
						return;
					}
					while (NJson.IsWhiteSpace(data[--len])) { }

					this.Value = data.Slice(first, ++len - first);
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
						this.Value = data.Slice(first, len - first);
						this.IsEmpty = true;
						return;
					}
					while (NJson.IsWhiteSpace(data[--len])) { }

					this.Value = data.Slice(first, ++len - first);
					this.IsEmpty = false;
					return;
				}
				case 'n':
				case 'N': {
					this.Type = JsonType.Null;
					this.Value = NJson.NULL.AsSpan();
					int len = data.Length;
					while (NJson.IsWhiteSpace(data[--len])) { }
					this.IsEmpty = false;
					if (++len - x == 4) {
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
					int len = data.Length;
					while (NJson.IsWhiteSpace(data[--len])) { }
					this.IsEmpty = false;
					if (++len - x == 4) {
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
					int len = data.Length;
					while (NJson.IsWhiteSpace(data[--len])) { }
					this.IsEmpty = false;
					if (++len - x == 5) {
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
				default: {
					this.Type = JsonType.Number;
					this.IsEmpty = false;
					int len = data.Length;

					while (NJson.IsWhiteSpace(data[--len])) { }
					this.Value = data.Slice(x, ++len - x);
					if (!double.TryParse(this.Value, out _)) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					return;
				}
			}
			throw new ArgumentException($"Parse failed (TryParse: {data.ToString()})", nameof(data));
		}

		public readonly nJson this[in ReadOnlySpan<char> key]
		{
			get
			{
				if (this.IsEmpty) {
					return Empty;
				}
				switch (this.Type) {
					case JsonType.Object:
						if (this.TryGetKey(in key, out nJson v)) {
							return v;
						}
						return Empty;
					default:
						throw new InvalidOperationException();
				}
			}
		}

		/// <summary>
		/// Single time access of index, work done after aquiring value is lost, use Enumerator for more persistent access
		/// </summary>
		/// <param name="index"></param>
		/// <returns></returns>
		/// <exception cref="IndexOutOfRangeException"></exception>
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

		public bool TryGetKey(in ReadOnlySpan<char> key, out nJson value) {
			if (this.Type != JsonType.Object) {
				value = nJson.Empty;
				return false;
			}
			int pathLen = key.Length;
			int nameLen = -1;

			int x = 0;
			int len = this.Value.Length;
			int y;
			int depth = 0;

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
					name = this.Value.Slice(y, x - y);
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
							depth++;
							break;
						case ']':
							depth--;
							break;
						case '}':
							if (--depth < 0) { // no comma found, process last segment
								goto ProcessJsonObject;
							}
							break;
						case ',':
							if (depth == 0) {
								goto ProcessJsonObject;
							}
							break;
					}
					x++;
				}

				ProcessJsonObject:
				int continuationPoint = x;
				while (NJson.IsWhiteSpace(this.Value[--x])) { }
				x++;
				if (found) {
					if (nameLen == pathLen) {
						value = new nJson(this.Value.Slice(y, x - y));
						return true;
					}
					else {
						if (new nJson(this.Value.Slice(y, x - y)).TryGetKey(key[++nameLen..], out value)) {
							return true;
						}
						return false;
					}
				}
				else if (continuationPoint == len) {
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
		public string GetStringDecoded => NJson.GetStringDecodedFromSpan(in this.Value);

		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public string TryGetString(in ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(in key, out nJson value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetString(in ReadOnlySpan<char> key, out string @out, bool decoded = true) {
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
		public double TryGetNumber(in ReadOnlySpan<char> key) => this.TryGetNumber(in key, out double value) ? value : double.NaN;
		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetNumber(in ReadOnlySpan<char> key, out double @out) {
			if (this.TryGetKey(in key, out nJson value) && value.Type == JsonType.Number) {
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
			if (this.TryGetKey(in key, out nJson value) && value.Type == JsonType.Number) {
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
		public bool TryGetBool(in ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(in key, out nJson value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

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
		public bool TryGetDateTime(in ReadOnlySpan<char> key, out DateTime @out) {
			if (this.TryGetKey(in key, out nJson value) && JsonType.DateTime.HasFlag(this.Type)) {
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

	[Serializable, Flags]
	public enum JsonType : byte {
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

	public readonly struct NJson : IEquatable<NJson>, IComparable<NJson> {
		public readonly struct NanoArray {
			public readonly static NanoArray Empty_body = new NanoArray(false);
			public static ref readonly NanoArray Empty => ref NanoArray.Empty_body;
			public readonly NJson[] Data;

			private NanoArray(bool _) {
				this.Data = Array.Empty<NJson>();
			}

			public NanoArray(params NJson[] data) {
				this.Data = data;
			}

			public ref readonly NJson this[int index]
			{
				get => ref this.Data[index];
			}

			public readonly int Length => this.Data.Length;

			public readonly Enumerator GetEnumerator() => new Enumerator(this);

			public readonly NJson[] Clone() => this.Length == 0 ? Array.Empty<NJson>() : (NJson[])this.Data.Clone();

			public readonly ReadOnlySpan<NJson> GetSpan => this.Data.AsSpan();

			public readonly string[] ToStringArray() {
				string[] array = new string[this.Length];
				for (int x = 0; x < this.Length; x++) {
					array[x] = this[x].GetStringDecoded;
				}
				return array;
			}

			public ref struct Enumerator {
				private readonly NanoArray owner;
				private int index;

				public readonly ref readonly NJson Current => ref this.owner[this.index];

				public Enumerator(in NanoArray owner) {
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

		private static NJson ParseJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> data) {
			if (key.Span.Trim().IsEmpty) {
				if (data.Span.Trim().IsEmpty) {
					return NJson.Empty;
				}
				return NJson.ParseJson(in data);
			}
			else {
				if (data.Span.Trim().IsEmpty) {
					return NJson.CreateNull(key);
				}
				return new NJson(in key, in data, -1);
			}
		}

		public static bool TryParseJson(string key, string data, out NJson parsed) {
			return NJson.TryParseJson(key.AsMemory(), data.AsMemory(), out parsed);
		}

		private static bool TryParseJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> data, out NJson parsed) {
			try {
				if (key.IsEmpty) {
					parsed = NJson.ParseJson(in data);
				}
				else {
					parsed = NJson.ParseJson(in key, in data);
				}
				if (parsed.Type == JsonType.Null) {
					return false;
				}
				return true;
			} catch {
				parsed = NJson.Empty;
				return false;
			}
		}

		public static NJson ParseJson(string data) {
			return NJson.ParseJson(data.AsMemory());
		}

		private static NJson ParseJson(in ReadOnlyMemory<char> data) {
			if (data.Span.Trim().IsEmpty) {
				return NJson.Empty;
			}
			return new NJson(ReadOnlyMemory<char>.Empty, in data, -1);
		}

		public static bool TryParseJson(string data, out NJson parsed) {
			return NJson.TryParseJson(data.AsMemory(), out parsed);
		}

		private static bool TryParseJson(in ReadOnlyMemory<char> data, out NJson parsed) {
			try {
				parsed = NJson.ParseJson(in data);
				if (parsed.Type == JsonType.Null) {
					return false;
				}
				return true;
			} catch {
				parsed = NJson.Empty;
				return false;
			}
		}

		public static NJson Pin(in nJson data) {
			if (data.IsNothing) {
				return NJson.Empty;
			}
			switch (data.Type) {
				case JsonType.Null:
					if (data.Key.IsEmpty) {
						return NJson.Empty;
					}
					else {
						return new NJson(data.Key.ToArray());
					}
				case JsonType.Object:
				case JsonType.Array: // nJson is on demand so this information hasnt been processed so translate as if fresh
					return NJson.ParseJson(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, data.Value.ToArray());
				case JsonType.String:
					return NJson.CreateString(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, data.Value.ToArray());
				case JsonType.Number:
					return NJson.CreateNumber(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, double.Parse(data.Value));
				case JsonType.Boolean:
					return NJson.CreateBool(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, bool.Parse(data.Value));
				default:
					throw new NotSupportedException();
			}
		}

		/// <summary>
		/// Remakes the existing NJson object, usually an internal node, by allocating the segments to arrays. Typically needed when you want to deallocate a large Json string container but keeping its smaller internal nodes
		/// </summary>
		/// <param name="data"></param>
		/// <returns></returns>
		/// <exception cref="NotSupportedException"></exception>
		public static NJson Pin(in NJson data) {
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
					return NJson.ParseJson(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray());
				case JsonType.String:
					return NJson.CreateString(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray());
				case JsonType.Number:
					return NJson.CreateNumber(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, double.Parse(data.GetValueAsSpan));
				case JsonType.Boolean:
					return NJson.CreateBool(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, bool.Parse(data.GetValueAsSpan));
				default:
					throw new NotSupportedException();
			}
		}

		public static NJson CreateArray(string key, NJson[] data, bool AllocateNewContainer = false) {
			return NJson.CreateArray(key.AsMemory(), AllocateNewContainer ? (NJson[])data.Clone() : data);
		}

		public static NJson CreateArray(string key, in NJson data) {
			return NJson.CreateArray(key.AsMemory(), in data);
		}

		private static NJson CreateArray(in ReadOnlyMemory<char> key, NJson[] data, bool AllocateNewContainer = false) {
			return new NJson(in key, JsonType.Array, AllocateNewContainer ? (NJson[])data.Clone() : data);
		}

		private static NJson CreateArray(in ReadOnlyMemory<char> key, in NJson data) {
			return new NJson(in key, JsonType.Array, data);
		}

		public static NJson CreateArray(NJson[] data, bool AllocateNewContainer = false) {
			return new NJson(JsonType.Array, AllocateNewContainer ? (NJson[])data.Clone() : data);
		}

		public static NJson CreateArray(in NJson data) {
			return new NJson(JsonType.Array, data);
		}

		public static NJson CreateObject(string key, NJson[] data, bool AllocateNewContainer = false) {
			return NJson.CreateObject(key.AsMemory(), data, AllocateNewContainer);
		}

		public static NJson CreateObject(string key, in NJson data) {
			return NJson.CreateObject(key.AsMemory(), in data);
		}

		private static NJson CreateObject(in ReadOnlyMemory<char> key, NJson[] data, bool AllocateNewContainer = false) {
			return new NJson(in key, JsonType.Object, AllocateNewContainer ? (NJson[])data.Clone() : data);
		}

		private static NJson CreateObject(in ReadOnlyMemory<char> key, in NJson data) {
			return new NJson(in key, JsonType.Object, data);
		}

		public static NJson CreateObject(NJson[] data, bool AllocateNewContainer = false) {
			return new NJson(JsonType.Object, AllocateNewContainer ? (NJson[])data.Clone() : data);
		}

		public static NJson CreateObject(in NJson data) {
			return new NJson(JsonType.Object, data);
		}

		public static NJson CreateString(string data) {
			return NJson.CreateString(data.AsMemory());
		}

		public static NJson CreateString(string key, string data) {
			return NJson.CreateString(key.AsMemory(), data.AsMemory());
		}

		private static NJson CreateString(in ReadOnlyMemory<char> data) {
			return NJson.CreateString(ReadOnlyMemory<char>.Empty, in data);
		}

		private static NJson CreateString(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> data) {
			return new NJson(in key, in data);
		}

		public static NJson CreateDateTime(in DateTime data) {
			return NJson.CreateDateTime(ReadOnlyMemory<char>.Empty, in data);
		}

		public static NJson CreateDateTime(string key, in DateTime data) {
			return NJson.CreateDateTime(key.AsMemory(), in data);
		}

		private static NJson CreateDateTime(in ReadOnlyMemory<char> key, in DateTime data) {
			return new NJson(in key, data.ToString("o").AsMemory());
		}

		/// <summary>
		/// Take an existing value and assign its values to a new object with a new key
		/// </summary>
		/// <param name="key"></param>
		/// <param name="data"></param>
		/// <returns></returns>
		public static NJson AssignKeyToValue(string key, in NJson data) {
			return NJson.AssignKeyToValue(key.AsMemory(), in data);
		}

		/// <summary>
		/// Take an existing value and assign its values to a new object with a new key
		/// </summary>
		/// <param name="key"></param>
		/// <param name="data"></param>
		/// <returns></returns>
		private static NJson AssignKeyToValue(in ReadOnlyMemory<char> key, in NJson data) {
			return new NJson(in key, in data);
		}

		public static NJson CreateBool(bool data) {
			return NJson.CreateBool(ReadOnlyMemory<char>.Empty, data);
		}

		public static NJson CreateBool(string key, bool data) {
			return NJson.CreateBool(key.AsMemory(), data);
		}

		private static NJson CreateBool(in ReadOnlyMemory<char> key, bool data) {
			return new NJson(in key, data);
		}

		public static NJson CreateNumber(double data) {
			return NJson.CreateNumber(ReadOnlyMemory<char>.Empty, data);
		}

		public static NJson CreateNumber(string key, double data) {
			return NJson.CreateNumber(key.AsMemory(), data);
		}

		private static NJson CreateNumber(in ReadOnlyMemory<char> key, double data) {
			return new NJson(in key, data);
		}

		public static ref readonly NJson CreateNull() {
			return ref NJson.Empty;
		}

		public static NJson CreateNull(string key) {
			return NJson.CreateNull(key.AsMemory());
		}

		private static NJson CreateNull(in ReadOnlyMemory<char> key) {
			return new NJson(in key);
		}

		private readonly static NJson Empty_Body = new NJson(ReadOnlyMemory<char>.Empty, ReadOnlyMemory<char>.Empty, -1);
		public static ref readonly NJson Empty => ref NJson.Empty_Body;

		public readonly JsonType Type;
		private readonly NanoArray InnerValues;
		private readonly ReadOnlyMemory<char> ReferenceData;
		private readonly ReadOnlyMemory<char> KeyData;
		private readonly int KeyHash => NJson.ComputeHash(this.KeyData.Span, out _);
		private readonly int KeyLen => this.GetKeyAsSpan.Length;

		private NJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> reference, int innerLength = -1) {
			this.KeyData = key;
			if (reference.IsEmpty) {
				this.Type = JsonType.Null;
				this.ReferenceData = ReadOnlyMemory<char>.Empty;
				this.InnerValues = NanoArray.Empty;
				return;
			}
			int len = reference.Length;
			int x = -1;
			ReadOnlySpan<char> data = reference.Span;
			while (NJson.IsWhiteSpace(data[++x])) { }
			while (NJson.IsWhiteSpace(data[--len])) { }
			len++;
			len -= x;
			ReadOnlyMemory<char> trimmedRef = reference.Slice(x, len);
			data = trimmedRef.Span;
			switch (data[0]) {
				case '"': {
					this.Type = JsonType.String;
					this.InnerValues = NanoArray.Empty;

					while (true) {
						if (data[--len] == '"') {
							break;
						}
					}
					if (len < 1) {
						this.ReferenceData = ReadOnlyMemory<char>.Empty;
					}
					else {
						this.ReferenceData = trimmedRef.Slice(1, len - 1);
					}
					return;
				}
				case '[': {
					this.Type = JsonType.Array;
					this.ReferenceData = trimmedRef;

					while (true) {
						if (data[--len] == ']') {
							break;
						}
					}
					x = 0;
					while (NJson.IsWhiteSpace(data[++x])) { }
					if (x == len) {
						this.InnerValues = NanoArray.Empty;
						return;
					}
					int lower = x;

					int depth = 0;
					if (innerLength == -1) {
						innerLength = 1; // we know there is atleast 1, and increases on ','
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
									depth++;
									break;
								case '}':
									depth--;
									break;
								case ']':
									if (depth == 0) {
										goto Fin;
									}
									depth--;
									break;
								case ',':
									if (depth == 0) {
										innerLength++;
									}
									break;
							}
							x++;
						}
					}
					Fin:
					NJson[] newValues = new NJson[innerLength];
					x = lower;
					int y = x;
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
									depth++;
									break;
								case ']':
								case '}':
									if (--depth < 0) {
										goto ProcessJsonObject;
									}
									break;
								case ',':
									if (depth == 1) {
										innerSize++;
									}
									else if (depth == 0) {
										goto ProcessJsonObject;
									}
									break;
							}
							x++;
						}

						ProcessJsonObject:
						newValues[index++] = new NJson(ReadOnlyMemory<char>.Empty, trimmedRef.Slice(y, x - y), innerSize);
						if (index == innerLength) {
							this.InnerValues = new NanoArray(newValues);
							return;
						}
						y = ++x;
					}
				}
				case '{': {
					this.Type = JsonType.Object;
					this.ReferenceData = trimmedRef;

					while (true) {
						if (data[--len] == '}') {
							break;
						}
					}

					if (len <= 0) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
					}
					x = 0;
					while (NJson.IsWhiteSpace(data[++x])) { }
					if (x == len) {
						this.InnerValues = NanoArray.Empty;
						return;
					}
					int lower = x;

					int depth = 0;
					if (innerLength == -1) {
						innerLength = 1; // we know there is atleast 1, and increases on ','
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
									depth++;
									break;
								case ']':
									depth--;
									break;
								case '}':
									if (depth == 0) {
										goto Fin;
									}
									depth--;
									break;
								case ',':
									if (depth == 0) {
										innerLength++;
									}
									break;
							}
							x++;
						}
					}
					Fin:
					NJson[] newValues = new NJson[innerLength];
					x = lower;
					int y;
					int index = 0;
					int innerSize;
					int nameL;
					int nameR;

					while (true) {
						while (true) {
							if (data[x] == '"') {
								break;
							}
							x++;
						}
						if (data[++x] == '"') {
							nameL = 0;
							nameR = 0;
						}
						else {
							nameL = x;
							while (true) {
								if (data[++x] == '"') {
									break;
								}
							}
							nameR = x - nameL;
						}

						while (true) {
							if (data[++x] == ':') {
								break;
							}
						}
						while (NJson.IsWhiteSpace(data[++x])) { }
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
									depth++;
									break;
								case ']':
								case '}':
									if (--depth < 0) { // no comma found, process last segment
										goto ProcessJsonObject;
									}
									break;
								case ',':
									if (depth == 1) {
										innerSize++;
									}
									else if (depth == 0) {
										goto ProcessJsonObject;
									}
									break;
							}
							x++;
						}

						ProcessJsonObject:
						newValues[index++] = new NJson(trimmedRef.Slice(nameL, nameR), trimmedRef.Slice(y, x - y), innerSize);
						if (index == innerLength) {
							this.InnerValues = new NanoArray(newValues);
							return;
						}
					}
				}
				case 'n':
				case 'N': {
					this.Type = JsonType.Null;
					this.InnerValues = NanoArray.Empty;
					if (len == 4) {
						char c = data[++x];
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
					if (len == 4) {
						char c = data[++x];
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

					if (len == 5) {
						char c = data[++x];
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

					this.ReferenceData = trimmedRef;
					x = 0;
					while (++x < len) {
						switch (data[x]) {
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
								continue;
							case '.':
								if (dec) {
									throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
								}
								dec = true;
								continue;
							case 'e':
							case 'E':
								if (E) {
									throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
								}
								else {
									char c = data[++x];
									if (c == '+' || c == '-') {
										E = true;
										continue;
									}
									else {
										throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
									}
								}
							default:
								throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(reference));
						}
					}
					return;
				}
			}
			throw new ArgumentException($"Parse failed (TryParse: {data.ToString()})", nameof(reference));
		}

		private NJson(in JsonType type, params NJson[] contents) : this(ReadOnlyMemory<char>.Empty, type, contents) { }

		private NJson(in ReadOnlyMemory<char> key, in JsonType type, params NJson[] contents) {
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

		private NJson(in ReadOnlyMemory<char> key, in NJson value) {
			this.KeyData = key;
			this.Type = value.Type;
			this.ReferenceData = value.ReferenceData;
			this.InnerValues = value.InnerValues;
		}

		private NJson(in ReadOnlyMemory<char> key, bool value) {
			this.KeyData = key;
			this.Type = JsonType.Boolean;
			this.ReferenceData = value ? bool.TrueString.AsMemory() : bool.FalseString.AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(in ReadOnlyMemory<char> key, double value) {
			this.KeyData = key;
			this.Type = JsonType.Number;
			this.ReferenceData = value.ToString().AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(in ReadOnlyMemory<char> key) {
			this.KeyData = key;
			this.Type = JsonType.Null;
			this.ReferenceData = NULL.AsMemory();
			this.InnerValues = NanoArray.Empty;
		}

		private NJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> value) {
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
		public readonly ref readonly NJson this[string path] => ref this[path.AsSpan()];

		/// <summary>
		/// Searchs the values for matching Key. Keys including '.' will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		/// <param name="key">Key or Path to desired value</param>
		/// <returns></returns>
		/// <exception cref="ArgumentException">Key was not found in object</exception>
		/// <exception cref="InvalidOperationException">NanoJson value is not an object, search not supported</exception>
		public readonly ref readonly NJson this[ReadOnlySpan<char> key]
		{
			get
			{
				if (this.Type == JsonType.Object) {
					int hash = NJson.ComputeHash(in key, out int pathLen);
					int innerCount = this.InnerValues.Length;
					for (int x = 0; x < innerCount; x++) {
						ref readonly NJson value = ref this.InnerValues[x];
						if (hash == value.KeyHash) {
							return ref value;
						}
						else {
							int len = value.KeyLen;
							if (pathLen > len && NJson.ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
								return ref value[key.Slice(++len)];
							}
						}
					}
				}

				return ref NJson.Empty;
			}
		}

		/// <summary>
		/// Get value at index of the contained NanoJson values
		/// </summary>
		/// <param name="index"></param>
		/// <returns></returns>
		/// <exception cref="IndexOutOfRangeException"></exception>
		public readonly ref readonly NJson this[int index]
		{
			get
			{
				switch (this.Type) {
					case JsonType.Object:
					case JsonType.Array:
						return ref this.InnerValues[index];
					default:
						throw new IndexOutOfRangeException();
				}
			}
		}

		[Flags]
		public enum ToStringFormat : byte {
			None = 0,
			Pretty = 0x1,
			TranslateUnicode = 0x2,
			LowerCaseBool = 0x4,

			Default = Pretty | TranslateUnicode,
			All = Pretty | TranslateUnicode | LowerCaseBool,
		}

		/// <summary>
		/// Format used by the basic <c>.ToString()</c>
		/// </summary>
		public static ToStringFormat Default_ToStringFormat = ToStringFormat.Pretty | ToStringFormat.TranslateUnicode;

		public readonly override string ToString() => this.ToString(Default_ToStringFormat);

		public readonly string ToString(in ToStringFormat format) {
			int count = 0;
			int indent = 0;
			this.CalculateStringSize(false, in format, ref count, ref indent);

			char[] buffer = ArrayPool<char>.Shared.Rent(count);
			indent = 0;
			int pos = 0;
			Span<char> data = buffer.AsSpan(0, count);
			this.ProcessString(false, in format, in data, ref indent, ref pos, INDENT_TABS.AsSpan());

			string builtString = data.ToString();

			ArrayPool<char>.Shared.Return(buffer);
			return builtString;
		}

		private readonly void CalculateStringSize(bool AsValue, in ToStringFormat format, ref int count, ref int indent) {
			switch (this.Type) {
				case JsonType.String:
					count += (format.HasFlag(ToStringFormat.TranslateUnicode) ? this.GetStringDecodeLength : this.GetValueAsSpan.Length) + 2;
					break;
				case JsonType.Null:
					count += 4;
					break;
				case JsonType.Number: {
					double d = this.GetNumber;
					d.TryFormat(stackalloc char[32], out int refSpanLen);
					count += refSpanLen;
					break;
				}
				case JsonType.Boolean:
					count += this.GetValueAsSpan.Length;
					break;
				case JsonType.Object: {
					int innerCount = this.InnerValues.Length;
					bool pretty = format.HasFlag(ToStringFormat.Pretty);
					if (pretty && !AsValue) {
						count += indent * INDENT_LEN;
					}
					count++;
					int x;

					if (innerCount == 0) {
						if (pretty) {
							count++;
						}
						count++;
						break;
					}
					indent++;
					if (pretty) {
						count += innerCount * ((indent * INDENT_LEN) + 6);
						for (x = 0; x < innerCount; x++) {
							ref readonly NJson value = ref this.InnerValues[x];
							count += value.KeyLen;
							value.CalculateStringSize(true, in format, ref count, ref indent);
						}
					}
					else {
						count += (innerCount * 5) - 1;
						for (x = 0; x < innerCount; x++) {
							ref readonly NJson value = ref this.InnerValues[x];
							count += value.KeyLen;
							value.CalculateStringSize(true, in format, ref count, ref indent);
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
					bool pretty = format.HasFlag(ToStringFormat.Pretty);
					if (pretty && !AsValue) {
						count += indent * INDENT_LEN;
					}
					count++;
					int x;

					if (innerCount == 0) {
						if (pretty) {
							count++;
						}
						count++;
					}
					else {
						indent++;
						if (pretty) {
							count += innerCount * 2;
							for (x = 0; x < innerCount; x++) {
								ref readonly NJson value = ref this.InnerValues[x];
								if (JsonType.Value.HasFlag(value.Type)) {
									count += indent * INDENT_LEN;
								}
								value.CalculateStringSize(false, in format, ref count, ref indent);
							}
						}
						else {
							count += innerCount - 1;
							for (x = 0; x < innerCount; x++) {
								this.InnerValues[x].CalculateStringSize(false, in format, ref count, ref indent);
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
		private readonly void ProcessString(bool AsValue, in ToStringFormat format, in Span<char> sb, ref int indent, ref int sbPos, in ReadOnlySpan<char> indentSpan) {
			switch (this.Type) {
				case JsonType.String: {
					sb[sbPos++] = '"';
					if (format.HasFlag(ToStringFormat.TranslateUnicode)) {
						NJson.RentStringDecodedIntoBufferFromSpan(this.GetValueAsSpan, sb, ref sbPos);
					}
					else {
						ReadOnlySpan<char> refSpan = this.GetValueAsSpan;
						int refSpanLen = refSpan.Length;
						refSpan.CopyTo(sb.Slice(sbPos, refSpanLen));
						sbPos += refSpanLen;
					}
					sb[sbPos++] = '"';
					break;
				}
				case JsonType.Null: {
					this.GetValueAsSpan.CopyTo(sb.Slice(sbPos, 4));
					sbPos += 4;
					break;
				}
				case JsonType.Number: {
					this.GetNumber.TryFormat(sb.Slice(sbPos), out int refSpanLen);
					sbPos += refSpanLen;
					break;
				}
				case JsonType.Boolean: {
					ReadOnlySpan<char> refSpan = this.GetValueAsSpan;
					int refSpanLen = refSpan.Length;
					int first = sbPos;
					refSpan.CopyTo(sb.Slice(sbPos, refSpanLen));
					sbPos += refSpanLen;
					if (format.HasFlag(ToStringFormat.LowerCaseBool)) {
						sb[first] = char.ToLower(refSpan[0]);
					}
					break;
				}
				case JsonType.Object: {
					int innerCount = this.InnerValues.Length;
					bool pretty = format.HasFlag(ToStringFormat.Pretty);
					int x;
					int y;
					if (pretty && !AsValue) {
						for (x = 0; x < indent; x++) {
							indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
							sbPos += INDENT_LEN;
						}
					}
					sb[sbPos++] = '{';

					if (innerCount == 0) {
						if (pretty) {
							sb[sbPos++] = ' ';
						}
						sb[sbPos++] = '}';
						break;
					}
					indent++;
					int limit = innerCount - 1;
					ReadOnlySpan<char> keySpan;
					int keyLen;
					if (pretty) {
						sb[sbPos++] = '\n';
						for (x = 0; x < limit; x++) {
							ref readonly NJson value = ref this.InnerValues[x];
							for (y = 0; y < indent; y++) {
								indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
								sbPos += INDENT_LEN;
							}
							sb[sbPos++] = '"';
							keySpan = value.GetKeyAsSpan;
							keyLen = keySpan.Length;
							keySpan.CopyTo(sb.Slice(sbPos, keyLen));
							sbPos += keyLen;
							sb[sbPos++] = '"';
							sb[sbPos++] = ':';
							sb[sbPos++] = ' ';
							value.ProcessString(true, in format, sb, ref indent, ref sbPos, in indentSpan);
							sb[sbPos++] = ',';
							sb[sbPos++] = '\n';
						}
						ref readonly NJson valueLast = ref this.InnerValues[x];
						for (y = 0; y < indent; y++) {
							indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
							sbPos += INDENT_LEN;
						}
						sb[sbPos++] = '"';
						keySpan = valueLast.GetKeyAsSpan;
						keyLen = keySpan.Length;
						keySpan.CopyTo(sb.Slice(sbPos, keyLen));
						sbPos += keyLen;
						sb[sbPos++] = '"';
						sb[sbPos++] = ':';
						sb[sbPos++] = ' ';
						valueLast.ProcessString(true, in format, sb, ref indent, ref sbPos, in indentSpan);
						sb[sbPos++] = '\n';

						indent--;
						for (x = 0; x < indent; x++) {
							indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
							sbPos += INDENT_LEN;
						}
					}
					else {
						for (x = 0; x < limit; x++) {
							ref readonly NJson value = ref this.InnerValues[x];
							sb[sbPos++] = '"';
							keySpan = value.GetKeyAsSpan;
							keyLen = keySpan.Length;
							keySpan.CopyTo(sb.Slice(sbPos, keyLen));
							sbPos += keyLen;
							sb[sbPos++] = '"';
							sb[sbPos++] = ':';
							sb[sbPos++] = ' ';
							value.ProcessString(true, in format, sb, ref indent, ref sbPos, in indentSpan);
							sb[sbPos++] = ',';
						}
						ref readonly NJson valueLast = ref this.InnerValues[x];
						sb[sbPos++] = '"';
						keySpan = valueLast.GetKeyAsSpan;
						keyLen = keySpan.Length;
						keySpan.CopyTo(sb.Slice(sbPos, keyLen));
						sbPos += keyLen;
						sb[sbPos++] = '"';
						sb[sbPos++] = ':';
						sb[sbPos++] = ' ';
						valueLast.ProcessString(true, in format, sb, ref indent, ref sbPos, in indentSpan);
					}

					sb[sbPos++] = '}';
					break;
				}
				case JsonType.Array: {
					int innerCount = this.InnerValues.Length;
					bool pretty = format.HasFlag(ToStringFormat.Pretty);
					int x;
					int y;
					if (pretty && !AsValue) {
						for (x = 0; x < indent; x++) {
							indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
							sbPos += INDENT_LEN;
						}
					}
					sb[sbPos++] = '[';

					if (innerCount == 0) {
						if (pretty) {
							sb[sbPos++] = ' ';
						}
						sb[sbPos++] = ']';
					}
					else {
						indent++;
						int limit = innerCount - 1;
						if (pretty) {
							sb[sbPos++] = '\n';
							for (x = 0; x < limit; x++) {
								ref readonly NJson value = ref this.InnerValues[x];
								if (JsonType.Value.HasFlag(value.Type)) {
									for (y = 0; y < indent; y++) {
										indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
										sbPos += INDENT_LEN;
									}
								}
								value.ProcessString(false, in format, sb, ref indent, ref sbPos, in indentSpan);
								sb[sbPos++] = ',';
								sb[sbPos++] = '\n';
							}
							ref readonly NJson valueLast = ref this.InnerValues[x];
							if (JsonType.Value.HasFlag(valueLast.Type)) {
								for (y = 0; y < indent; y++) {
									indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
									sbPos += INDENT_LEN;
								}
							}
							valueLast.ProcessString(false, in format, sb, ref indent, ref sbPos, in indentSpan);
							sb[sbPos++] = '\n';

							indent--;
							for (x = 0; x < indent; x++) {
								indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
								sbPos += INDENT_LEN;
							}
						}
						else {
							for (x = 0; x < limit; x++) {
								ref readonly NJson value = ref this.InnerValues[x];
								value.ProcessString(false, in format, sb, ref indent, ref sbPos, in indentSpan);
								sb[sbPos++] = ',';
							}
							ref readonly NJson valueLast = ref this.InnerValues[x];
							valueLast.ProcessString(false, in format, sb, ref indent, ref sbPos, in indentSpan);
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

		public readonly int GetStringDecodeLength => NJson.GetStringDecodeLengthFromSpan(this.GetValueAsSpan);

		/// <summary>
		/// Get the decoded string value of the object
		/// </summary>
		public readonly string GetStringDecoded => NJson.GetStringDecodedFromSpan(this.GetValueAsSpan);

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
		public readonly string TryGetString(in ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(key, out NJson value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public readonly bool TryGetString(in ReadOnlySpan<char> key, out string @out, bool decoded = true) {
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
		public readonly double GetNumber => double.Parse(this.GetValueAsSpan);

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
		public readonly double TryGetNumber(in ReadOnlySpan<char> key) => this.TryGetKey(in key, out NJson value) ? value.GetNumber : double.NaN;
		/// <summary>
		/// Try to get the numerical value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public readonly bool TryGetNumber(in ReadOnlySpan<char> key, out double @out) {
			if (this.TryGetKey(in key, out NJson value) && value.Type == JsonType.Number) {
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
			if (this.TryGetKey(in key, out NJson value) && value.Type == JsonType.Number) {
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
		public readonly bool GetBool => bool.TryParse(this.GetValueAsSpan, out bool value) && value;

		/// <summary>
		/// Get the inner values of this Object/Array as a string array
		/// </summary>
		public readonly string[] ToStringArray => this.InnerValues.ToStringArray();

		/// <summary>
		/// Gets this value as a System.DateTime using TryParse
		/// </summary>
		public readonly DateTime GetDateTime => DateTime.Parse(this.GetValueAsSpan);

		/// <summary>
		/// Gets this value as a System.DateTime using TryParse
		/// </summary>
		public readonly DateTime TryGetDateTime(string key) => this.TryGetKey(key, out NJson value) ? value.GetDateTime : DateTime.MinValue;

		/// <summary>
		/// Gets this value as a System.DateTime using TryParse
		/// </summary>
		public readonly bool TryGetDateTime(string key, out DateTime @out) => this.TryGetDateTime(key.AsSpan(), out @out);

		/// <summary>
		/// Gets this value as a System.DateTime using TryParse
		/// </summary>
		public readonly bool TryGetDateTime(in ReadOnlySpan<char> key, out DateTime @out) {
			if (this.TryGetKey(in key, out NJson value) && JsonType.DateTime.HasFlag(value.Type)) {
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
		public readonly bool TryGetBool(in ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(in key, out NJson value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

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
			return this.KeyHash == NJson.ComputeHash(key, out _);
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
		public readonly bool TryGetKey(in ReadOnlySpan<char> key, out NJson found) {
			if (this.Type == JsonType.Object) {
				int hash = NJson.ComputeHash(in key, out int pathLen);
				int innerCount = this.InnerValues.Length;
				for (int x = 0; x < innerCount; x++) {
					ref readonly NJson value = ref this.InnerValues[x];
					if (hash == value.KeyHash) {
						found = value;
						return true;
					}
					else {
						int len = value.KeyLen;
						if (pathLen > len && NJson.ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
							if (value.TryGetKey(key[++len..], out found)) {
								return true;
							}
						}
					}
				}
			}

			found = NJson.Empty;
			return false;
		}

		/// <summary>
		/// <c>ref readonly</c> version of TryGetKey, returns the value as a reference with the success value in the out
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key reference</returns>
		public readonly ref readonly NJson TryGetKeyRef(string key, out bool found) => ref this.TryGetKeyRef(key.AsSpan(), out found);

		/// <summary>
		/// <c>ref readonly</c> version of TryGetKey, returns the value as a reference with the success value in the out
		/// </summary>
		/// <param name="key"></param>
		/// <returns>Key reference</returns>
		public readonly ref readonly NJson TryGetKeyRef(ReadOnlySpan<char> key, out bool found) {
			if (this.Type == JsonType.Object) {
				int hash = NJson.ComputeHash(in key, out int pathLen);
				int innerCount = this.InnerValues.Length;
				for (int x = 0; x < innerCount; x++) {
					ref readonly NJson value = ref this.InnerValues[x];
					if (hash == value.KeyHash) {
						found = true;
						return ref value;
					}
					int len = value.KeyLen;
					if (pathLen > len && NJson.ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
						ref readonly NJson candidate = ref value.TryGetKeyRef(key.Slice(++len), out found);
						if (found) {
							return ref candidate;
						}
					}
				}
			}
			found = false;
			return ref NJson.Empty;
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		public readonly NJson GetKeyOrEmpty(string key) => this.GetKeyOrEmpty(key.AsSpan());
		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		public readonly NJson GetKeyOrEmpty(in ReadOnlySpan<char> key) => this.TryGetKey(in key, out NJson found) ? found : NJson.Empty;

		public readonly NanoArray.Enumerator GetEnumerator() => this.InnerValues.GetEnumerator();

		public readonly override bool Equals(object obj) => obj is NJson other && this.Equals(other);
		public readonly bool Equals(NJson other) {
			if (this.Type.Equals(other.Type)
				&& this.InnerValues.Data.Equals(other.InnerValues.Data)
				&& this.KeyHash == other.KeyHash
				&& this.GetValueAsSpan.Equals(other.GetValueAsSpan, StringComparison.Ordinal)) {
				return true;
			}
			return false;
		}
		public static bool operator ==(in NJson left, in NJson right) {
			return left.Equals(right);
		}

		public static bool operator !=(in NJson left, in NJson right) {
			return !(left == right);
		}

		public readonly override int GetHashCode() {
			return HashCode.Combine(this.Type, this.InnerValues.Data, this.ReferenceData, this.KeyHash);
		}

		public static implicit operator NJson(in nJson span) {
			return NJson.Pin(in span);
		}
		public static implicit operator nJson(in NJson self) {
			if (self.KeyData.IsEmpty) {
				return new nJson(self.GetValueAsSpan);
			}
			else {
				return new nJson(self.GetKeyAsSpan, self.GetValueAsSpan);
			}
		}

		public static implicit operator NJson[](in NJson container) {
			return container.InnerValues.Clone();
		}

		internal static T GetConvertedValue<T>(double value) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
			unchecked {
				switch (System.Type.GetTypeCode(typeof(T))) {
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
			if (len <= 256) {// fast-path for small strings: stackalloc to avoid ArrayPool rent
				Span<char> stackBuffer = stackalloc char[len];
				NJson.RentStringDecodedIntoBufferFromSpan(in data, in stackBuffer, ref x);
				return stackBuffer.Slice(0, x).ToString();
			}
			char[] buffer = ArrayPool<char>.Shared.Rent(len);
			Span<char> bufSpan = buffer.AsSpan(0, len);
			NJson.RentStringDecodedIntoBufferFromSpan(in data, in bufSpan, ref x);
			string decodedValue = bufSpan.Slice(0, x).ToString();
			ArrayPool<char>.Shared.Return(buffer);
			return decodedValue;
		}

		public static int GetStringDecodeLengthFromSpan(in ReadOnlySpan<char> data) {
			int count = 0;
			int x = 0;
			int len = data.Length;
			while (x < len) {
				if (data[x] == '\\' && ++x < len) {
					if (data[x] == 'u') {
						if (x + 4 >= len) {
							break;
						}
						else {
							x += 4;
						}
					}
				}
				count++;
				x++;
			}
			return count;
		}

		public static void RentStringDecodedIntoBufferFromSpan(in ReadOnlySpan<char> data, in Span<char> buffer, ref int sbPos) {
			int x = 0;
			int len = data.Length;
			char c;

			while (x < len) {
				c = data[x];
				if (c == '\\' && ++x < len) {
					c = data[x];
					switch (c) {
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
							buffer[sbPos++] = (char)((NJson.ReadHexNumber(data[++x]) * 4096)
									+ (NJson.ReadHexNumber(data[++x]) * 256)
									+ (NJson.ReadHexNumber(data[++x]) * 16)
									+ NJson.ReadHexNumber(data[++x]));
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
							buffer[sbPos++] = c;
							break;
					}
				}
				else {
					buffer[sbPos++] = c;
				}
				x++;
			}
		}

		public static bool IsWhiteSpace(char character) {
			return character <= 32;
		}

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

		public static int ComputeHash(in ReadOnlySpan<char> data, out int len) {
			int hash = 17; // Initial hash value
			len = data.Length;
			for (int x = 0; x < len; x++) {
				hash = hash * 31 + data[x]; // Combine hash with character
			}
			return hash; // Return the final hash value
		}

		public int CompareTo(NJson other) {
			return this.KeyHash.CompareTo(other.KeyHash);
		}
	}

	public static class NJsonExtensions {
		public static NJson ToJsonObject(this NJson[] objects, string key = "") {
			if (string.IsNullOrWhiteSpace(key)) {
				return NJson.CreateObject(objects);
			}
			else {
				return NJson.CreateObject(key, objects);
			}
		}

		public static NJson ToJsonArray(this NJson[] array, string key = "") {
			if (string.IsNullOrWhiteSpace(key)) {
				return NJson.CreateArray(array);
			}
			else {
				return NJson.CreateArray(key, array);
			}
		}

		public static NJson ToJsonArray(this string[] strings, string key = "") {
			int len = strings.Length;
			NJson[] array = new NJson[len];
			for (int x = 0; x < len; x++) {
				array[x] = NJson.CreateString(strings[x]);
			}
			if (string.IsNullOrWhiteSpace(key)) {
				return NJson.CreateArray(array);
			}
			else {
				return NJson.CreateArray(key, array);
			}
		}
	}
}
