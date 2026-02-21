# NanoJson

[![Static Badge](https://img.shields.io/badge/Nuget-Latest-teal?logo=nuget)](https://www.nuget.org/packages/NanoJson)
[![Static Badge](https://img.shields.io/badge/Github-repo-blue?logo=github)](https://github.com/ScoredOne/NanoJson)

NetStandard 2.1  

Small and simple Json Parser to minimise memory allocation of Json.  
(Formerly NanoJson.NJson ext)

Theory:
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily such as additional string allocations.

// * Summary *

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6937/22H2/2022Update)
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores
.NET SDK 9.0.307
  [Host]   : .NET 5.0.17 (5.0.1722.21314), X64 RyuJIT AVX2 [AttachedDebugger]
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 <br>

Job=.NET 9.0  Runtime=.NET 9.0

| Method                   | format             | Mean          | Error        | StdDev       | Gen0    | Gen1    | Gen2    | Allocated |
|------------------------- |------------------- |--------------:|-------------:|-------------:|--------:|--------:|--------:|----------:|
| NanoJson_JsonSpan*       | JustParse          |      33.57 ns |     0.028 ns |     0.025 ns |       - |       - |       - |         - |
| NanoJson_JsonMemory      | JustParse          | 241,214.67 ns | 1,626.939 ns | 1,442.239 ns |  4.3945 |  0.4883 |       - |   74928 B |
| System.Text.Json         | JustParse          | 135,288.05 ns |   156.955 ns |   131.064 ns |  5.6152 |  0.4883 |       - |   95696 B |
| LightJson                | JustParse          | 338,335.19 ns | 1,570.335 ns | 1,468.892 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson           | JustParse          | 504,082.22 ns | 2,872.296 ns | 2,398.497 ns | 36.1328 | 25.3906 |       - |  618136 B |
|                          |                    |               |              |              |         |         |         |           |
| NanoJson_JsonSpan        | ParseToString      | 494,223.63 ns | 2,135.551 ns | 1,997.596 ns | 30.2734 | 30.2734 | 30.2734 |   95674 B |
| NanoJson_JsonMemory      | ParseToString      | 386,639.30 ns | 2,341.740 ns | 2,075.891 ns | 30.2734 | 30.2734 | 30.2734 |  170588 B |
| System.Text.Json         | ParseToString      | 256,067.47 ns | 1,815.913 ns | 1,609.759 ns | 27.3438 | 27.3438 | 27.3438 |  183708 B |
| LightJson                | ParseToString      | 500,614.54 ns | 3,644.779 ns | 3,409.328 ns | 41.9922 | 20.5078 |       - |  714936 B |
| NewtonsoftJson           | ParseToString      | 636,750.81 ns | 2,468.194 ns | 2,308.750 ns | 54.6875 | 53.7109 | 27.3438 |  804049 B |
|                          |                    |               |              |              |         |         |         |           |
| NanoJson_JsonSpan**      | ParseReformat      | 440,311.67 ns | 1,899.435 ns | 1,683.800 ns |  5.8594 |  0.4883 |       - |  105744 B |
| NanoJson_JsonMemory      | ParseReformat      | 323,021.04 ns | 6,389.027 ns | 6,274.879 ns |  9.7656 |  1.9531 |       - |  166680 B |
| System.Text.Json         | ParseReformat      | 313,285.68 ns | 2,553.895 ns | 2,132.618 ns | 23.4375 |  9.7656 |       - |  415664 B |
| LightJson                | ParseReformat      | 453,900.13 ns | 3,647.930 ns | 3,046.186 ns | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson           | ParseReformat      | 632,137.40 ns | 6,303.847 ns | 5,896.622 ns | 54.6875 | 26.3672 |       - |  930152 B |
|                          |                    |               |              |              |         |         |         |           |
| NanoJson_JsonSpan        | ParseToSingleValue |  40,851.67 ns |   138.109 ns |   129.187 ns |       - |       - |       - |      56 B |
| NanoJson_JsonMemory      | ParseToSingleValue | 224,043.80 ns | 4,447.696 ns | 4,758.985 ns |  4.3945 |  0.4883 |       - |   74984 B |
| System.Text.Json         | ParseToSingleValue | 137,499.61 ns |   411.618 ns |   343.719 ns |  5.8594 |  0.9766 |       - |   99968 B |
| LightJson                | ParseToSingleValue | 334,460.62 ns | 1,935.745 ns | 1,810.697 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson           | ParseToSingleValue | 460,828.79 ns | 4,095.251 ns | 3,830.700 ns | 36.6211 | 26.3672 |       - |  618136 B |

*JsonSpan doesnt parse on data insersion. Hence the results of JustParse.
**JsonSpan as it cant store the nodes, parses and reads the nodes as JsonSpan then pins them to memory as JsonMemory before running ToString

Test format:  
JustParse			= Create object of library and parse json file into it. No output requested from object.  
ParseToString		= Create object of library and parse json file into it. Take created object and request string of object.  
ParseReformat		= Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object.  
ParseToSingleValue	= Create object of library and parse json file into it. Receives index and string path and aquires a single value.  

## JsonMemory
Minimal allocation without compromise on time.  
Access constructors via static JsonMemory.ParseJson ext.  
Object and Array types both accessible via indexer, dictionary is not implemented.  
Object and Array types both accessible via foreach, IEnumerator not supported, uses custom Enumerator.  
Object type can also be accessed via key search. Furthermore, you can include a seperator (for example 'object.value') in your string and it will aquire the internal value.  

### Creating JsonMemory Types
```CS
public static JsonMemory ParseJson(string key, string data); // Translate string data into JsonMemory
public static JsonMemory ParseJson(string data);

public static bool TryParseJson(string key, string data, out JsonMemory parsed);
public static bool TryParseJson(string data, out JsonMemory parsed);

public static JsonMemory CreateArray(string key, JsonMemory[] data); // Add Existing values to a Array
public static JsonMemory CreateArray(JsonMemory[] data);

public static JsonMemory CreateObject(string key, JsonMemory[] data); // Add Existing values to a Object
public static JsonMemory CreateObject(JsonMemory[] data);

public static JsonMemory CreateString(string key, string data);
public static JsonMemory CreateString(string data);

public static JsonMemory CreateBool(string key, bool data);
public static JsonMemory CreateBool(bool data);

public static JsonMemory CreateNumber(string key, double data);
public static JsonMemory CreateNumber(double data);

public static JsonMemory AssignKeyToValue(string key, JsonMemory data); // To create a new JsonMemory value using existing data and a new name
```
Statics are provided to create the objects you want. Constructors are private to maintain consistent object construction due to recursive loops.  

## JsonMemory arrays
JsonMemory objects and arrays are built using standard arrays, create the objects you want into the container

## JsonSpan
The absolute smallest, parse on demand Json.  
ref struct for methods quick access json data and extract what you need.  
Index support not included, intended for direct access for specific variables.  
Foreach support included just like JsonMemory, however speed is a bit slower than JsonMemory due to parse on demand nature.  

### Creating JsonSpan Types
```CS
private void Function(string jsonData) {
    JsonSpan js = new JsonSpan(jsonData);
}
```
With on demand parsing, the reference target goes in and it will use it later.  

## Value Access  

### Foreach  

Both JsonMemory and JsonSpan support foreach iteration with a custom Enumerator, ref struct based for no allocation looping.  
Both Array and Objects can be looped or indexed to aquire its inner values. As a dictionary isnt used, larger scoped Objects will be slower to access than traditional hash code methods but on small scale objects the speed is negligable.    
```CS
private void Foreach(string jsonData) {
    JsonMemory JMemory = JsonMemory.ParseJson(jsonData);
    JsonSpan JSpan = new JsonSpan(jsonData);
    foreach (JsonMemory nano in JMemory) {
        ...
    }
    foreach (JsonSpan n in JSpan) {
        ...
    }
}
```  

### Key Index 

Both JsonMemory and JsonSpan support key searching and key path searching for Json Objects (e.g Object -> Object -> Object -> Value).  

```CS
private void Key(string jsonData) {
    JsonMemory JMemory = JsonMemory.ParseJson(jsonData);
    JsonSpan JSpan = new JsonSpan(jsonData);

    JsonMemory JMemoryValue1 = JMemory["name"];
    JsonMemory JMemoryValue2 = JMemory["name.name.value"]; // The same as JMemory["name"]["name"]["value"]
    JsonMemory JMemoryValue3 = JMemory["name.name"][2]["value"]; // The same as JMemory["name"]["name"][2]["value"]

    JsonSpan JSpanValue1 = JSpan["name"];
    JsonSpan JSpanValue2 = JSpan["name.name.value"]; // The same as JSpan["name"]["name"]["value"]
    JsonSpan JSpanValue3 = JSpan["name.name"][2]["value"]; // The same as JSpan["name"]["name"][2]["value"]
}
``` 

### Numeric Index  

JsonMemory as it uses arrays allows indexing support, this means index support is available for both Json Objects and Arrays.

```CS
private void Index(string jsonData) {
    JsonMemory JMemory = JsonMemory.ParseJson(jsonData);
    JsonMemory JSpan = JMemory[0];

    JsonSpan JSpan = new JsonSpan(jsonData);
    JsonSpan JSpanIndex = JSpan[0];
}
```  

JsonSpan has index support however it should only be used to access a single value from the data, otherwise using the Enumerator is recommended.
The Enumerator stores the index and read position while directly asking for the index in a JsonSpan object will not be remembered and will need to start the search from scratch.
For similar functionality with JsonSpan, the Enumerator has been expanded and included a TryGetIndex method.  

```CS
private void Index(string jsonData) {
    JsonSpan JSpan = new JsonSpan(jsonData);
    JsonSpan.Enumerator JSpan_Enum = JSpan.GetEnumerator();
    if (JSpan_Enum.TryGetIndex(2, out JsonSpan JSpan2)) {
        ...
    }
    // From here, JSpan_Enum is at index 2, going backwards calls Reset() internally, going forward continues where it left off at 2
}
```  

No arrays are used in JsonSpan so compromises must be made and the indexer was one of them.

## Extracting Json

### ToString

ToString has been implemented to provide the current structure of JsonMemory or JsonSpan as a String.
The body and sub values are evaluated to determine the character total required, rents the space and writes to it. Then gives the result to new String to create the output.  
Resulting in a body direct to string implimentation, no streams or string builders.  
```CS
private void GetString(string jsonData) {
    JsonMemory JMemory = JsonMemory.ParseJson(jsonData);
    string jsonString = JMemory.ToString();
}
```  
An enum is also provided for additional settings. Using ToStringFormat you can combine options for ToString.  
```CS
private void GetString(string jsonData) {
    JsonMemory JMemory = JsonMemory.ParseJson(jsonData);
    string jsonString = JMemory.ToString(ToStringFormat.Pretty | ToStringFormat.TranslateUnicode);

    // JMemory.ToString() == JMemory.ToString(NanoJsonStatics.Default_ToStringFormat)
}
```  

### TryGet*

Indexers are designed to throw, values that you are absolutely certain exist are aquired through indexers.  
To allow null returns on attempts, Try methods are provided. When failing to aquire a value, it will return the static Empty value (As structs cant be null).  
Provide the key just like the indexer.  

```CS
private void TryGet(string jsonData) {
    JsonMemory JMemory = JsonMemory.ParseJson(jsonData);
    JsonSpan JSpan = new JsonSpan(jsonData);

    if (JMemory.TryGetKey("name.name.value", out JsonMemory value)) {
        ...
    } else {
        ... // value == JsonMemory.Empty
    }

    if (JSpan.TryGetKey("name.name.value", out JsonSpan value)) {
        ...
    } else {
        ... // value == JsonSpan.Empty
    }
}
``` 

### Creating new Json Strings

Using the static methods you can piece together new Json's

```CS
Console.WriteLine(
    new JsonMemory[] {
		JsonMemory.CreateBool("BoolKey", true),
		JsonMemory.CreateNumber("NumberKey", 84526),
		JsonMemory.CreateNull("NullKey"),
		JsonMemory.CreateDateTime("DateTimeKey", DateTime.Now),
		JsonMemory.CreateString("StringKey", "Test String Words"),
		JsonMemory.CreateArray("ArrayKey", new JsonMemory[] {
			JsonMemory.CreateString(null, "Array 1"),
			JsonMemory.CreateBool(null, true),
			JsonMemory.CreateDateTime(null, DateTime.MaxValue),
			JsonMemory.CreateNull(null),
			JsonMemory.CreateNumber(null, 4892),
		})
    }.ToJsonObject().ToString()
);

Result::
{
   "BoolKey": True,
   "NumberKey": 84526,
   "NullKey": null,
   "DateTimeKey": "2026-01-26T00:15:51.7484061+00:00",
   "StringKey": "Test String Words",
   "ArrayKey": [
      "Array 1",
      True,
      "9999-12-31T23:59:59.9999999",
      null,
      4892
   ]
}
```

### Extensions

JsonMemory Arrays have a couple of extensions provided to quickly contain the contenteds within either an Object or Array.
An optional key can be provided, so it works the same as if calling the equivolvent static method.

```CS
private JsonMemory ArrayToObject() {
    return new JsonMemory[] {
        JsonMemory.CreateString("Item1", "Content One"),
        JsonMemory.CreateString("Item2", "Content Two"),
        JsonMemory.CreateString("Item3", "Content Three"),
    }.ToJsonObject();
}

Result ToString::
{
   "Item1": "Content One",
   "Item2": "Content Two",
   "Item3": "Content Three"
}
```

### Ref readonly Support

As JsonMemory is a struct, its been built to support ref readonly when accessing parsed contents.
Access the contents via its reference, stopping unneccessary copies.

```CS
private void TryGet(string jsonData) {
    JsonMemory JMemory = JsonMemory.ParseJson(jsonData);
    foreach (ref readonly JsonMemory value in JMemory) {
        ref readonly JsonMemory innerValue = ref value[0];
        ...
    }
}
``` 