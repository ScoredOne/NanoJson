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

	#region ### JsonSpan ###

	public readonly ref struct JsonSpan {

		public ref struct Enumerator {
			private readonly JsonSpan owner;
			private readonly int len;

			private JsonSpan current;

			private int index;
			private int x;
			private int y;
			private int depth;
			private int arrayPos;

			internal Enumerator(in JsonSpan owner) {
				this.owner = owner;
				this.len = owner.Value.Length;
				this.current = JsonSpan.Empty;

				this.index = -1;
				this.x = 0;
				this.y = -1;
				this.depth = 0;
				this.arrayPos = 0;
			}

			public readonly JsonSpan Current => this.current;

			public JsonSpan this[int index]
			{
				get
				{
					if (this.TryGetIndex(index, out JsonSpan value)) {
						return value;
					}
					throw new IndexOutOfRangeException();
				}
			}

			public bool TryGetIndex(int index, out JsonSpan value) {
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
					value = JsonSpan.Empty;
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
							while (JsonMemory.IsWhiteSpace(this.owner.Value[++this.x])) { }
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
							while (JsonMemory.IsWhiteSpace(this.owner.Value[--this.x])) { }
							this.x++;
							if (!name.IsEmpty) {
								if (this.y > this.x) {
									this.x = this.y;
								}
								this.current = new JsonSpan(name, this.owner.Value[this.y..this.x]);
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
							while (JsonMemory.IsWhiteSpace(this.owner.Value[this.x - 1])) {
								this.x--;
							}
							if (this.arrayPos == this.index) {
								if (this.y > this.x) {
									this.x = this.y;
								}
								this.current = new JsonSpan(this.owner.Value[this.y..this.x]);
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

		public static JsonSpan Empty => new JsonSpan(true);

		public readonly JsonType Type;
		public readonly ReadOnlySpan<char> Key;
		public readonly ReadOnlySpan<char> Value;
		public readonly bool IsEmpty;

		public readonly bool IsNothing => this.Key == ReadOnlySpan<char>.Empty && this.Value == ReadOnlySpan<char>.Empty;

		private JsonSpan(bool _) {
			this.Type = JsonType.Null;
			this.Key = ReadOnlySpan<char>.Empty;
			this.Value = ReadOnlySpan<char>.Empty;
			this.IsEmpty = true;
		}

		public JsonSpan(string key, string value) : this(key.AsSpan(), value.AsSpan()) { }
		public JsonSpan(in Span<char> key, in Span<char> value) : this((ReadOnlySpan<char>)key, (ReadOnlySpan<char>)value) { }
		public JsonSpan(in ReadOnlySpan<char> key, in ReadOnlySpan<char> value) : this(value) {
			this.Key = key;
		}

		public JsonSpan(string data) : this(data.AsSpan()) { }
		public JsonSpan(in Span<char> data) : this((ReadOnlySpan<char>)data) { }
		public JsonSpan(in ReadOnlySpan<char> read) {
			this.Key = ReadOnlySpan<char>.Empty;
			int len = read.Length;
			int x = -1;
			while (JsonMemory.IsWhiteSpace(read[++x])) { }
			while (JsonMemory.IsWhiteSpace(read[--len])) { }
			len++;
			len -= x;
			ReadOnlySpan<char> data = read.Slice(x, len);
			if (data.IsEmpty) {
				this.Type = JsonType.Null;
				this.Value = ReadOnlySpan<char>.Empty;
				this.IsEmpty = true;
				return;
			}

			x = 0;
			switch (data[0]) {
				case '"': {
					this.Type = JsonType.String;
					x = data.Length;

					while (true) {
						if (data[--x] == '"') {
							break;
						}
					}

					int z = x - 1;
					if (z < 0) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					else if (z == 0) {
						this.Value = ReadOnlySpan<char>.Empty;
						this.IsEmpty = true;
					}
					else {
						this.Value = data.Slice(1, z);
						this.IsEmpty = false;
					}
					return;
				}
				case '[': {
					this.Type = JsonType.Array;

					while (true) {
						if (data[--len] == ']') {
							break;
						}
					}

					if (len <= x) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					while (JsonMemory.IsWhiteSpace(data[++x])) { }
					if (x == len++) {
						this.Value = ReadOnlySpan<char>.Empty;
						this.IsEmpty = true;
						return;
					}

					this.Value = data;
					this.IsEmpty = false;
					return;
				}
				case '{': {
					this.Type = JsonType.Object;

					while (true) {
						if (data[--len] == '}') {
							break;
						}
					}

					if (len <= x) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					while (JsonMemory.IsWhiteSpace(data[++x])) { }
					if (x == len++) {
						this.Value = ReadOnlySpan<char>.Empty;
						this.IsEmpty = true;
						return;
					}

					this.Value = data;
					this.IsEmpty = false;
					return;
				}
				case 'n':
				case 'N': {
					this.Type = JsonType.Null;
					this.Value = JsonMemory.NULL.AsSpan();
					this.IsEmpty = false;
					if (len == 4) {
						char c = data[++x];
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
					if (len == 4) {
						char c = data[++x];
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
					if (len == 5) {
						char c = data[++x];
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
					this.Value = data;
					if (!double.TryParse(this.Value, out _)) {
						throw new ArgumentException($"Parse failed (JsonType: {Enum.GetName(typeof(JsonType), this.Type)}, TryParse: {data.ToString()})", nameof(data));
					}
					return;
				}
			}
			throw new ArgumentException($"Parse failed (TryParse: {data.ToString()})", nameof(data));
		}

		public readonly JsonSpan this[in ReadOnlySpan<char> key]
		{
			get
			{
				if (this.IsEmpty) {
					return Empty;
				}
				switch (this.Type) {
					case JsonType.Object:
						if (this.TryGetKey(in key, out JsonSpan v)) {
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
		public readonly JsonSpan this[int index]
		{
			get
			{
				if (this.IsEmpty) {
					throw new IndexOutOfRangeException("Body is Empty");
				}
				if (this.GetEnumerator().TryGetIndex(index, out JsonSpan v)) {
					return v;
				}
				throw new IndexOutOfRangeException();
			}
		}

		public bool TryGetKey(in ReadOnlySpan<char> key, out JsonSpan value) {
			if (this.Type != JsonType.Object) {
				value = JsonSpan.Empty;
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
				while (JsonMemory.IsWhiteSpace(this.Value[++x])) { }
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
				while (JsonMemory.IsWhiteSpace(this.Value[--x])) { }
				x++;
				if (found) {
					if (nameLen == pathLen) {
						value = new JsonSpan(this.Value.Slice(y, x - y));
						return true;
					}
					else {
						if (new JsonSpan(this.Value.Slice(y, x - y)).TryGetKey(key[++nameLen..], out value)) {
							return true;
						}
						return false;
					}
				}
				else if (continuationPoint == len) {
					value = JsonSpan.Empty;
					return false;
				}
			}
		}

		public readonly Enumerator GetEnumerator() => new Enumerator(this);

		/// <summary>
		/// Get the string value as-is in relation to this object
		/// </summary>
		public readonly string GetStringLiteral => this.Value.ToString();

		/// <summary>
		/// Get the decoded string value of the object
		/// </summary>
		public readonly string GetStringDecoded => JsonMemory.GetStringDecodedFromSpan(in this.Value);

		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public string TryGetString(in ReadOnlySpan<char> key, bool decoded = true) => this.TryGetKey(in key, out JsonSpan value) && value.Type == JsonType.String ? (decoded ? value.GetStringDecoded : value.GetStringLiteral) : string.Empty;
		/// <summary>
		/// Try to get the string value of the object at path
		/// </summary>
		/// <param name="key"></param>
		/// <returns></returns>
		public bool TryGetString(in ReadOnlySpan<char> key, out string @out, bool decoded = true) {
			if (this.TryGetKey(key, out JsonSpan value) && value.Type == JsonType.String) {
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
			return double.TryParse(this.Value, out double value) ? JsonMemory.GetConvertedValue<T>(value) : default;
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
		public bool TryGetNumber<T>(in ReadOnlySpan<char> key, out T @out) where T : struct, IComparable, IComparable<T>, IConvertible, IEquatable<T>, IFormattable {
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
		public bool TryGetBool(string key) => this.TryGetKey(key, out JsonSpan value) && value.GetBool;

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
		public bool TryGetBool(in ReadOnlySpan<char> key, out bool @out) => this.TryGetKey(in key, out JsonSpan value) ? (@out = value.Type == JsonType.Boolean && value.GetBool) : (@out = false);

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
			if (this.TryGetKey(in key, out JsonSpan value) && JsonType.DateTime.HasFlag(this.Type)) {
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

	public readonly struct JsonMemory : IEquatable<JsonMemory>, IComparable<JsonMemory> {

		public readonly JsonType Type;
		public readonly JsonMemory[] InnerValues;
		private readonly ReadOnlyMemory<char> ReferenceData;
		private readonly ReadOnlyMemory<char> KeyData;
		private readonly int KeyHash => JsonMemory.ComputeHash(this.KeyData.Span, out _);
		private readonly int KeyLen => this.GetKeyAsSpan.Length;

		private JsonMemory(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> reference) {
			this.KeyData = key;
			if (reference.IsEmpty) {
				this.Type = JsonType.Null;
				this.ReferenceData = ReadOnlyMemory<char>.Empty;
				this.InnerValues = Array.Empty<JsonMemory>();
				return;
			}
			int len = reference.Length;
			int x = -1;
			ReadOnlySpan<char> data = reference.Span;
			while (JsonMemory.IsWhiteSpace(data[++x])) { }
			while (JsonMemory.IsWhiteSpace(data[--len])) { }
			len -= x - 1;
			ReadOnlyMemory<char> trimmedRef = reference.Slice(x, len);
			data = trimmedRef.Span;
			switch (data[0]) {
				case '"': {
					this.Type = JsonType.String;
					this.InnerValues = Array.Empty<JsonMemory>();

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
					while (JsonMemory.IsWhiteSpace(data[++x])) { }
					if (x == len) {
						this.InnerValues = Array.Empty<JsonMemory>();
						return;
					}
					JsonMemory[] buffer = ArrayPool<JsonMemory>.Shared.Rent(16);
					int depth = 0;
					int bufPos = 0;
					int left = 1;
					int right = 1;
					while (true) {
						switch (data[right]) {
							case '"':
								while (true) {
									if (data[++right] == '"') {
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
									buffer[bufPos++] = new JsonMemory(ReadOnlyMemory<char>.Empty, trimmedRef.Slice(left, right - left));
									goto ReadComplete;
								}
								depth--;
								break;
							case ',':
								if (depth == 0) {
									buffer[bufPos++] = new JsonMemory(ReadOnlyMemory<char>.Empty, trimmedRef.Slice(left, right - left));
									left = ++right;
									JsonMemory.EnsureBufferCapacity(bufPos + 1, ref buffer);
									continue;
								}
								break;
						}
						right++;
					}
					ReadComplete:
					this.InnerValues = buffer.AsSpan(0, bufPos).ToArray();
					ArrayPool<JsonMemory>.Shared.Return(buffer);
					return;
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
					while (JsonMemory.IsWhiteSpace(data[++x])) { }
					if (x == len) {
						this.InnerValues = Array.Empty<JsonMemory>();
						return;
					}

					JsonMemory[] buffer = ArrayPool<JsonMemory>.Shared.Rent(16);
					int depth = 0;
					int bufPos = 0;
					int left = 1;
					int right = 1;
					int nameL = 0;
					int nameR = 0;

					while (true) {
						while (true) {
							if (data[right] == '"') {
								break;
							}
							right++;
						}
						if (data[++right] == '"') {
							nameL = 0;
							nameR = 0;
						}
						else {
							nameL = right;
							while (true) {
								if (data[++right] == '"') {
									break;
								}
							}
							nameR = right - nameL;
						}

						while (true) {
							if (data[++right] == ':') {
								break;
							}
						}
						while (JsonMemory.IsWhiteSpace(data[++right])) { }
						left = right;

						while (true) {
							switch (data[right]) {
								case '"':
									while (true) {
										if (data[++right] == '"') {
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
									if (depth == 0) { // no comma found, process last segment
										goto ReadComplete;
									}
									depth--;
									break;
								case ',':
									if (depth == 0) {
										goto ProcessJsonObject;
									}
									break;
							}
							right++;
						}

						ReadComplete:
						buffer[bufPos++] = new JsonMemory(trimmedRef.Slice(nameL, nameR), trimmedRef.Slice(left, right - left));
						break;

						ProcessJsonObject:
						buffer[bufPos++] = new JsonMemory(trimmedRef.Slice(nameL, nameR), trimmedRef.Slice(left, right - left));
						JsonMemory.EnsureBufferCapacity(bufPos + 1, ref buffer);
					}

					this.InnerValues = buffer.AsSpan(0, bufPos).ToArray();
					ArrayPool<JsonMemory>.Shared.Return(buffer);
					return;
				}
				case 'n':
				case 'N': {
					this.Type = JsonType.Null;
					this.InnerValues = Array.Empty<JsonMemory>();
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
					this.InnerValues = Array.Empty<JsonMemory>();
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
					this.InnerValues = Array.Empty<JsonMemory>();

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
				case '+':
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
					this.InnerValues = Array.Empty<JsonMemory>();

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

		private JsonMemory(in JsonType type, params JsonMemory[] contents) : this(ReadOnlyMemory<char>.Empty, type, contents) { }

		private JsonMemory(in ReadOnlyMemory<char> key, JsonType type, params JsonMemory[] contents) {
			switch (type) {
				case JsonType.Array:
				case JsonType.Object:
					this.Type = type;
					this.InnerValues = contents;
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
			this.InnerValues = value.InnerValues;
		}

		private JsonMemory(in ReadOnlyMemory<char> key, bool value) {
			this.KeyData = key;
			this.Type = JsonType.Boolean;
			this.ReferenceData = value ? bool.TrueString.AsMemory() : bool.FalseString.AsMemory();
			this.InnerValues = Array.Empty<JsonMemory>();
		}

		private JsonMemory(in ReadOnlyMemory<char> key, double value) {
			this.KeyData = key;
			this.Type = JsonType.Number;
			this.ReferenceData = value.ToString().AsMemory();
			this.InnerValues = Array.Empty<JsonMemory>();
		}

		private JsonMemory(in ReadOnlyMemory<char> key) {
			this.KeyData = key;
			this.Type = JsonType.Null;
			this.ReferenceData = NULL.AsMemory();
			this.InnerValues = Array.Empty<JsonMemory>();
		}

		private JsonMemory(in ReadOnlyMemory<char> key, in string value) {
			this.KeyData = key;
			this.Type = JsonType.String;
			this.ReferenceData = value.AsMemory();
			this.InnerValues = Array.Empty<JsonMemory>();
		}

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
					int hash = JsonMemory.ComputeHash(in key, out int pathLen);
					int innerCount = this.InnerValues.Length;
					for (int x = innerCount - 1; x >= 0; x--) {
						ref readonly JsonMemory value = ref this.InnerValues[x];
						if (hash == value.KeyHash) {
							return ref value;
						}
						else {
							int len = value.KeyLen;
							if (pathLen > len && JsonMemory.ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
								return ref value[key.Slice(++len)];
							}
						}
					}
				}

				return ref JsonMemory.Empty;
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
						return ref this.InnerValues[index];
					default:
						throw new IndexOutOfRangeException();
				}
			}
		}

		[Serializable, Flags]
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

		public readonly string ToString(ToStringFormat format) {
			int count = 0;
			int indent = 0;
			this.CalculateStringSize(false, format, ref count, ref indent);

			indent = 0;
			int pos = 0;
			string builtString;
			if (count <= 256) { // fast-path for small strings: stackalloc to avoid ArrayPool rent
				Span<char> data = stackalloc char[count];
				this.ProcessString(false, format, in data, ref indent, ref pos, INDENT_TABS.AsSpan());
				builtString = data.ToString();
			}
			else {
				char[] buffer = ArrayPool<char>.Shared.Rent(count);
				Span<char> data = buffer.AsSpan(0, count);
				this.ProcessString(false, format, in data, ref indent, ref pos, INDENT_TABS.AsSpan());
				builtString = data.ToString();
				ArrayPool<char>.Shared.Return(buffer);
			}
			return builtString;
		}

		private readonly void CalculateStringSize(bool AsValue, ToStringFormat format, ref int count, ref int indent) {
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
						for (x = innerCount - 1; x >= 0; x--) {
							ref readonly JsonMemory value = ref this.InnerValues[x];
							count += value.KeyLen;
							value.CalculateStringSize(true, format, ref count, ref indent);
						}
					}
					else {
						count += (innerCount * 5) - 1;
						for (x = innerCount - 1; x >= 0; x--) {
							ref readonly JsonMemory value = ref this.InnerValues[x];
							count += value.KeyLen;
							value.CalculateStringSize(true, format, ref count, ref indent);
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
							for (x = innerCount - 1; x >= 0; x--) {
								ref readonly JsonMemory value = ref this.InnerValues[x];
								if (JsonType.Value.HasFlag(value.Type)) {
									count += indent * INDENT_LEN;
								}
								value.CalculateStringSize(false, format, ref count, ref indent);
							}
						}
						else {
							count += innerCount - 1;
							for (x = innerCount - 1; x >= 0; x--) {
								this.InnerValues[x].CalculateStringSize(false, format, ref count, ref indent);
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
		private readonly void ProcessString(bool AsValue, ToStringFormat format, in Span<char> sb, ref int indent, ref int sbPos, in ReadOnlySpan<char> indentSpan) {
			switch (this.Type) {
				case JsonType.String: {
					sb[sbPos++] = '"';
					if (format.HasFlag(ToStringFormat.TranslateUnicode)) {
						JsonMemory.TranslateUnicodeIntoBufferFromSpan(this.GetValueAsSpan, sb, ref sbPos);
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
						for (x = indent - 1; x >= 0; x--) {
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
							ref readonly JsonMemory value = ref this.InnerValues[x];
							for (y = indent - 1; y >= 0; y--) {
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
							value.ProcessString(true, format, sb, ref indent, ref sbPos, in indentSpan);
							sb[sbPos++] = ',';
							sb[sbPos++] = '\n';
						}
						ref readonly JsonMemory valueLast = ref this.InnerValues[x];
						for (y = indent - 1; y >= 0; y--) {
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
						valueLast.ProcessString(true, format, sb, ref indent, ref sbPos, in indentSpan);
						sb[sbPos++] = '\n';

						indent--;
						for (x = indent - 1; x >= 0; x--) {
							indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
							sbPos += INDENT_LEN;
						}
					}
					else {
						for (x = 0; x < limit; x++) {
							ref readonly JsonMemory value = ref this.InnerValues[x];
							sb[sbPos++] = '"';
							keySpan = value.GetKeyAsSpan;
							keyLen = keySpan.Length;
							keySpan.CopyTo(sb.Slice(sbPos, keyLen));
							sbPos += keyLen;
							sb[sbPos++] = '"';
							sb[sbPos++] = ':';
							sb[sbPos++] = ' ';
							value.ProcessString(true, format, sb, ref indent, ref sbPos, in indentSpan);
							sb[sbPos++] = ',';
						}
						ref readonly JsonMemory valueLast = ref this.InnerValues[x];
						sb[sbPos++] = '"';
						keySpan = valueLast.GetKeyAsSpan;
						keyLen = keySpan.Length;
						keySpan.CopyTo(sb.Slice(sbPos, keyLen));
						sbPos += keyLen;
						sb[sbPos++] = '"';
						sb[sbPos++] = ':';
						sb[sbPos++] = ' ';
						valueLast.ProcessString(true, format, sb, ref indent, ref sbPos, in indentSpan);
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
						for (x = indent - 1; x >= 0; x--) {
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
								ref readonly JsonMemory value = ref this.InnerValues[x];
								if (JsonType.Value.HasFlag(value.Type)) {
									for (y = indent - 1; y >= 0; y--) {
										indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
										sbPos += INDENT_LEN;
									}
								}
								value.ProcessString(false, format, sb, ref indent, ref sbPos, in indentSpan);
								sb[sbPos++] = ',';
								sb[sbPos++] = '\n';
							}
							ref readonly JsonMemory valueLast = ref this.InnerValues[x];
							if (JsonType.Value.HasFlag(valueLast.Type)) {
								for (y = indent - 1; y >= 0; y--) {
									indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
									sbPos += INDENT_LEN;
								}
							}
							valueLast.ProcessString(false, format, sb, ref indent, ref sbPos, in indentSpan);
							sb[sbPos++] = '\n';

							indent--;
							for (x = indent - 1; x >= 0; x--) {
								indentSpan.CopyTo(sb.Slice(sbPos, INDENT_LEN));
								sbPos += INDENT_LEN;
							}
						}
						else {
							for (x = 0; x < limit; x++) {
								ref readonly JsonMemory value = ref this.InnerValues[x];
								value.ProcessString(false, format, sb, ref indent, ref sbPos, in indentSpan);
								sb[sbPos++] = ',';
							}
							ref readonly JsonMemory valueLast = ref this.InnerValues[x];
							valueLast.ProcessString(false, format, sb, ref indent, ref sbPos, in indentSpan);
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

		public readonly int GetStringDecodeLength => JsonMemory.GetStringDecodeLengthFromSpan(this.GetValueAsSpan);

		/// <summary>
		/// Get the decoded string value of the object
		/// </summary>
		public readonly string GetStringDecoded => JsonMemory.GetStringDecodedFromSpan(this.GetValueAsSpan);

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

		private readonly JsonMemory[] Clone() => this.InnerLength == 0 ? Array.Empty<JsonMemory>() : (JsonMemory[])this.InnerValues.Clone();


		/// <summary>
		/// Get the values contained inside This object but as a new array
		/// </summary>
		public readonly JsonMemory[] GetCopyOfInsideValues => this.InnerLength == 0 ? Array.Empty<JsonMemory>() : (JsonMemory[])this.InnerValues.Clone();

		/// <summary>
		/// Get the values contained inside This object
		/// </summary>
		public readonly ReadOnlySpan<JsonMemory> GetInsideValues => this.InnerValues.AsSpan();

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
			if (this.TryGetKey(in key, out JsonMemory value) && JsonType.DateTime.HasFlag(value.Type)) {
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
			return this.KeyHash == JsonMemory.ComputeHash(key, out _);
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
				int hash = JsonMemory.ComputeHash(in key, out int pathLen);
				int innerCount = this.InnerValues.Length;
				for (int x = innerCount - 1; x >= 0; x--) {
					ref readonly JsonMemory value = ref this.InnerValues[x];
					if (hash == value.KeyHash) {
						found = value;
						return true;
					}
					else {
						int len = value.KeyLen;
						if (pathLen > len && JsonMemory.ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
							if (value.TryGetKey(key[++len..], out found)) {
								return true;
							}
						}
					}
				}
			}

			found = JsonMemory.Empty;
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
				int hash = JsonMemory.ComputeHash(in key, out int pathLen);
				int innerCount = this.InnerValues.Length;
				for (int x = innerCount - 1; x >= 0; x--) {
					ref readonly JsonMemory value = ref this.InnerValues[x];
					if (hash == value.KeyHash) {
						found = true;
						return ref value;
					}
					int len = value.KeyLen;
					if (pathLen > len && JsonMemory.ComputeHash(key.Slice(0, len), out _) == value.KeyHash) {
						ref readonly JsonMemory candidate = ref value.TryGetKeyRef(key.Slice(++len), out found);
						if (found) {
							return ref candidate;
						}
					}
				}
			}
			found = false;
			return ref JsonMemory.Empty;
		}

		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		public readonly JsonMemory GetKeyOrEmpty(string key) => this.GetKeyOrEmpty(key.AsSpan());
		/// <summary>
		/// Searchs the values for matching Key. Keys including seperators (e.g 'object.value') will start searching inside of subsiquent objects to find desired Key.
		/// </summary>
		public readonly JsonMemory GetKeyOrEmpty(in ReadOnlySpan<char> key) => this.TryGetKey(in key, out JsonMemory found) ? found : JsonMemory.Empty;

		public readonly ReadOnlySpan<JsonMemory>.Enumerator GetEnumerator() => ((ReadOnlySpan<JsonMemory>)this.InnerValues.AsSpan()).GetEnumerator();

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
			return JsonMemory.Pin(in span);
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

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
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
			if (len <= 256) { // fast-path for small strings: stackalloc to avoid ArrayPool rent
				Span<char> stackBuffer = stackalloc char[len];
				JsonMemory.TranslateUnicodeIntoBufferFromSpan(in data, in stackBuffer, ref x);
				return stackBuffer.Slice(0, x).ToString();
			}
			char[] buffer = ArrayPool<char>.Shared.Rent(len);
			Span<char> bufSpan = buffer.AsSpan(0, len);
			JsonMemory.TranslateUnicodeIntoBufferFromSpan(in data, in bufSpan, ref x);
			string decodedValue = bufSpan.Slice(0, x).ToString();
			ArrayPool<char>.Shared.Return(buffer);
			return decodedValue;
		}

		public static int GetStringDecodeLengthFromSpan(in ReadOnlySpan<char> data) {
			int count = 0;
			int len = data.Length;
			int x = len;
			char current;
			while (--x >= 0) {
				current = data[x];
				if (current == '\\') {
					if (x + 4 < len && data[x + 1] == 'u') {
						count -= 4;
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
						ref readonly char current = ref data[x];
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
								buffer[sbPos++] = (char)((JsonMemory.ReadHexNumber(data[++x]) * 4096)
										+ (JsonMemory.ReadHexNumber(data[++x]) * 256)
										+ (JsonMemory.ReadHexNumber(data[++x]) * 16)
										+ JsonMemory.ReadHexNumber(data[++x]));
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

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public static void EnsureBufferCapacity<T>(int neededCapacity, ref T[] buffer) {
			if (neededCapacity >= buffer.Length) {
				T[] newArray = ArrayPool<T>.Shared.Rent(neededCapacity);
				buffer.CopyTo(newArray, 0);
				ArrayPool<T>.Shared.Return(buffer);
				buffer = newArray;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public static bool IsWhiteSpace(char character) {
			return character <= 32;
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
			for (int x = len - 1; x >= 0; x--) {
				hash = hash * 31 + data[x]; // Combine hash with character
			}
			return hash; // Return the final hash value
		}

		public int CompareTo(JsonMemory other) {
			return this.KeyHash.CompareTo(other.KeyHash);
		}

		private const string INDENT_TABS = "   ";
		private const int INDENT_LEN = 3;
		public const string NULL = "null";

		private readonly static JsonMemory Empty_Body = new JsonMemory(ReadOnlyMemory<char>.Empty, ReadOnlyMemory<char>.Empty);
		public static ref readonly JsonMemory Empty => ref JsonMemory.Empty_Body;

		public static JsonMemory ParseJson(string key, string data) {
			return JsonMemory.ParseJson(key.AsMemory(), data.AsMemory());
		}

		private static JsonMemory ParseJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> data) {
			if (key.Span.Trim().IsEmpty) {
				if (data.Span.Trim().IsEmpty) {
					return JsonMemory.Empty;
				}
				return JsonMemory.ParseJson(in data);
			}
			else {
				if (data.Span.Trim().IsEmpty) {
					return JsonMemory.CreateNull(key);
				}
				return new JsonMemory(in key, in data);
			}
		}

		public static bool TryParseJson(string key, string data, out JsonMemory parsed) {
			return JsonMemory.TryParseJson(key.AsMemory(), data.AsMemory(), out parsed);
		}

		private static bool TryParseJson(in ReadOnlyMemory<char> key, in ReadOnlyMemory<char> data, out JsonMemory parsed) {
			try {
				if (key.IsEmpty) {
					parsed = JsonMemory.ParseJson(in data);
				}
				else {
					parsed = JsonMemory.ParseJson(in key, in data);
				}
				if (parsed.Type == JsonType.Null) {
					return false;
				}
				return true;
			} catch {
				parsed = JsonMemory.Empty;
				return false;
			}
		}

		public static JsonMemory ParseJson(string data) {
			return JsonMemory.ParseJson(data.AsMemory());
		}

		private static JsonMemory ParseJson(in ReadOnlyMemory<char> data) {
			if (data.Span.Trim().IsEmpty) {
				return JsonMemory.Empty;
			}
			return new JsonMemory(ReadOnlyMemory<char>.Empty, in data);
		}

		public static bool TryParseJson(string data, out JsonMemory parsed) {
			return JsonMemory.TryParseJson(data.AsMemory(), out parsed);
		}

		private static bool TryParseJson(in ReadOnlyMemory<char> data, out JsonMemory parsed) {
			try {
				parsed = JsonMemory.ParseJson(in data);
				if (parsed.Type == JsonType.Null) {
					return false;
				}
				return true;
			} catch {
				parsed = JsonMemory.Empty;
				return false;
			}
		}

		public static JsonMemory Pin(in JsonSpan data) {
			if (data.IsNothing) {
				return JsonMemory.Empty;
			}
			switch (data.Type) {
				case JsonType.Null:
					if (data.Key.IsEmpty) {
						return JsonMemory.Empty;
					}
					else {
						return new JsonMemory(data.Key.ToArray());
					}
				case JsonType.Object:
				case JsonType.Array: // JsonSpan is on demand so this information hasnt been processed so translate as if fresh
					return JsonMemory.ParseJson(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, data.Value.ToArray());
				case JsonType.String:
					return JsonMemory.CreateString(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, data.Value.ToString());
				case JsonType.Number:
					return JsonMemory.CreateNumber(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, double.Parse(data.Value));
				case JsonType.Boolean:
					return JsonMemory.CreateBool(data.Key.IsEmpty ? data.Key.ToArray() : ReadOnlyMemory<char>.Empty, bool.Parse(data.Value));
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
						return JsonMemory.Empty;
					}
					else {
						return new JsonMemory(data.KeyData.ToArray());
					}
				case JsonType.Object:
				case JsonType.Array: // Inner bodies need re-parsing as the originals reference the same allocated memory and we want it to point to a new area
					return JsonMemory.ParseJson(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToArray());
				case JsonType.String:
					return JsonMemory.CreateString(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, data.ReferenceData.ToString());
				case JsonType.Number:
					return JsonMemory.CreateNumber(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, double.Parse(data.GetValueAsSpan));
				case JsonType.Boolean:
					return JsonMemory.CreateBool(data.KeyData.IsEmpty ? data.KeyData.ToArray() : ReadOnlyMemory<char>.Empty, bool.Parse(data.GetValueAsSpan));
				default:
					throw new NotSupportedException();
			}
		}

		public static JsonMemory CreateArray(string key, JsonMemory[] data, bool AllocateNewContainer = false) {
			return JsonMemory.CreateArray(key.AsMemory(), AllocateNewContainer ? (JsonMemory[])data.Clone() : data);
		}

		public static JsonMemory CreateArray(string key, in JsonMemory data) {
			return JsonMemory.CreateArray(key.AsMemory(), in data);
		}

		private static JsonMemory CreateArray(in ReadOnlyMemory<char> key, JsonMemory[] data, bool AllocateNewContainer = false) {
			return new JsonMemory(in key, JsonType.Array, AllocateNewContainer ? (JsonMemory[])data.Clone() : data);
		}

		private static JsonMemory CreateArray(in ReadOnlyMemory<char> key, in JsonMemory data) {
			return new JsonMemory(in key, JsonType.Array, data);
		}

		public static JsonMemory CreateArray(JsonMemory[] data, bool AllocateNewContainer = false) {
			return new JsonMemory(JsonType.Array, AllocateNewContainer ? (JsonMemory[])data.Clone() : data);
		}

		public static JsonMemory CreateArray(in JsonMemory data) {
			return new JsonMemory(JsonType.Array, data);
		}

		public static JsonMemory CreateObject(string key, JsonMemory[] data, bool AllocateNewContainer = false) {
			return JsonMemory.CreateObject(key.AsMemory(), data, AllocateNewContainer);
		}

		public static JsonMemory CreateObject(string key, in JsonMemory data) {
			return JsonMemory.CreateObject(key.AsMemory(), in data);
		}

		private static JsonMemory CreateObject(in ReadOnlyMemory<char> key, JsonMemory[] data, bool AllocateNewContainer = false) {
			return new JsonMemory(in key, JsonType.Object, AllocateNewContainer ? (JsonMemory[])data.Clone() : data);
		}

		private static JsonMemory CreateObject(in ReadOnlyMemory<char> key, in JsonMemory data) {
			return new JsonMemory(in key, JsonType.Object, data);
		}

		public static JsonMemory CreateObject(JsonMemory[] data, bool AllocateNewContainer = false) {
			return new JsonMemory(JsonType.Object, AllocateNewContainer ? (JsonMemory[])data.Clone() : data);
		}

		public static JsonMemory CreateObject(in JsonMemory data) {
			return new JsonMemory(JsonType.Object, data);
		}

		public static JsonMemory CreateString(string data) {
			return JsonMemory.CreateString(ReadOnlyMemory<char>.Empty, data);
		}

		public static JsonMemory CreateString(string key, string data) {
			return JsonMemory.CreateString(key.AsMemory(), data);
		}

		private static JsonMemory CreateString(in ReadOnlyMemory<char> key, string data) {
			return new JsonMemory(in key, in data);
		}

		public static JsonMemory CreateDateTime(in DateTime data) {
			return JsonMemory.CreateDateTime(ReadOnlyMemory<char>.Empty, in data);
		}

		public static JsonMemory CreateDateTime(string key, in DateTime data) {
			return JsonMemory.CreateDateTime(key.AsMemory(), in data);
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
			return JsonMemory.AssignKeyToValue(key.AsMemory(), in data);
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
			return JsonMemory.CreateBool(ReadOnlyMemory<char>.Empty, data);
		}

		public static JsonMemory CreateBool(string key, bool data) {
			return JsonMemory.CreateBool(key.AsMemory(), data);
		}

		private static JsonMemory CreateBool(in ReadOnlyMemory<char> key, bool data) {
			return new JsonMemory(in key, data);
		}

		public static JsonMemory CreateNumber(double data) {
			return JsonMemory.CreateNumber(ReadOnlyMemory<char>.Empty, data);
		}

		public static JsonMemory CreateNumber(string key, double data) {
			return JsonMemory.CreateNumber(key.AsMemory(), data);
		}

		private static JsonMemory CreateNumber(in ReadOnlyMemory<char> key, double data) {
			return new JsonMemory(in key, data);
		}

		public static ref readonly JsonMemory CreateNull() {
			return ref JsonMemory.Empty;
		}

		public static JsonMemory CreateNull(string key) {
			return JsonMemory.CreateNull(key.AsMemory());
		}

		private static JsonMemory CreateNull(in ReadOnlyMemory<char> key) {
			return new JsonMemory(in key);
		}

	}

	#endregion

	#region ### JsonType ###

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

	#endregion

	#region ### MemoryJsonExtensions ###

	public static class MemoryJsonExtensions {
		public static JsonMemory ToJsonObject(this JsonMemory[] objects, string key = "") {
			if (string.IsNullOrWhiteSpace(key)) {
				return JsonMemory.CreateObject(objects);
			}
			else {
				return JsonMemory.CreateObject(key, objects);
			}
		}

		public static JsonMemory ToJsonArray(this JsonMemory[] array, string key = "") {
			if (string.IsNullOrWhiteSpace(key)) {
				return JsonMemory.CreateArray(array);
			}
			else {
				return JsonMemory.CreateArray(key, array);
			}
		}

		public static JsonMemory ToJsonArray(this string[] strings, string key = "") {
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
