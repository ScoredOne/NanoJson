# NanoJson

[![Static Badge](https://img.shields.io/badge/Nuget-Latest-teal?logo=nuget)](https://www.nuget.org/packages/NanoJson)
[![Static Badge](https://img.shields.io/badge/Github-repo-blue?logo=github)](https://github.com/ScoredOne/NanoJson)

NetStandard 2.1  

Small and simple Json Parser to minimise memory allocation of Json.  
(Formerly NanoJson.NJson)

Theory:
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily such as additional string allocations.

// * Summary *

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.7058/22H2/2022Update)
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores
.NET SDK 9.0.307
  [Host]   : .NET 5.0.17 (5.0.1722.21314), X64 RyuJIT AVX2 [AttachedDebugger]
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 <br>

Job=.NET 9.0  Runtime=.NET 9.0

| Method                        | format             | Mean          | Error        | StdDev        | Gen0    | Gen1    | Gen2    | Allocated |
|------------------------------ |------------------- |--------------:|-------------:|--------------:|--------:|--------:|--------:|----------:|
| NanoJson_JsonSpan*            | JustParse          |      36.29 ns |     0.498 ns |      0.465 ns |       - |       - |       - |         - |
| NanoJson_JsonMemory + Dispose | JustParse          | 124,227.65 ns |   452.766 ns |    423.518 ns |       - |       - |       - |    3456 B |
| NanoJson_JsonMemory + GC      | JustParse          | 196,887.26 ns | 3,235.243 ns |  2,867.959 ns |  2.1973 |  1.9531 |       - |   39515 B |
| SystemTextJson_Node           | JustParse          | 135,697.67 ns |   980.113 ns |    916.798 ns |  5.6152 |  0.9766 |       - |   95728 B |
| SystemTextJson_Document       | JustParse          | 126,375.47 ns |   350.699 ns |    328.044 ns |       - |       - |       - |      72 B |
| LightJson                     | JustParse          | 325,623.83 ns | 3,038.998 ns |  2,842.681 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson                | JustParse          | 461,109.36 ns | 4,229.823 ns |  3,956.579 ns | 36.6211 | 26.3672 |       - |  618136 B |
|                               |                    |               |              |               |         |         |         |           |
| NanoJson_JsonSpan             | ParseToString      | 388,158.01 ns | 1,700.398 ns |  1,590.554 ns | 30.2734 | 30.2734 | 30.2734 |   95650 B |
| NanoJson_JsonMemory + Dispose | ParseToString      | 245,318.71 ns | 1,898.457 ns |  1,775.818 ns |  7.3242 |  7.3242 |  7.3242 |   99132 B |
| NanoJson_JsonMemory + GC      | ParseToString      | 299,559.71 ns | 5,927.404 ns |  4,949.651 ns |  1.4648 |  0.9766 |  0.4883 |  118820 B |
| SystemTextJson_Node           | ParseToString      | 252,599.67 ns | 1,540.461 ns |  1,365.578 ns | 27.3438 | 27.3438 | 27.3438 |  183740 B |
| SystemTextJson_Document       | ParseToString      | 178,358.62 ns |   712.417 ns |    631.539 ns | 35.6445 | 35.6445 | 35.6445 |  114308 B |
| LightJson                     | ParseToString      | 484,912.44 ns | 4,502.171 ns |  3,991.057 ns | 41.9922 | 20.5078 |       - |  714936 B |
| NewtonsoftJson                | ParseToString      | 635,841.91 ns | 4,301.538 ns |  4,023.661 ns | 54.6875 | 53.7109 | 27.3438 |  804049 B |
|                               |                    |               |              |               |         |         |         |           |
| NanoJson_JsonSpan**           | ParseReformat      | 474,490.63 ns |   425.867 ns |    355.619 ns |  6.3477 |  0.9766 |       - |  112040 B |
| NanoJson_JsonMemory + Dispose | ParseReformat      | 207,559.79 ns |   910.481 ns |    851.665 ns |  5.8594 |       - |       - |  101512 B |
| NanoJson_JsonMemory + GC      | ParseReformat      | 361,368.33 ns | 6,953.995 ns | 10,193.076 ns |  7.3242 |  3.9063 |       - |  122819 B |
| SystemTextJson_Node           | ParseReformat      | 323,244.58 ns | 1,673.573 ns |  1,397.509 ns | 23.4375 |  9.7656 |       - |  415696 B |
| SystemTextJson_Document       | ParseReformat      | 244,963.39 ns | 1,005.115 ns |    839.316 ns |  7.8125 |       - |       - |  149864 B |
| LightJson                     | ParseReformat      | 462,181.82 ns | 4,036.050 ns |  3,775.323 ns | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson                | ParseReformat      | 612,235.65 ns | 4,297.326 ns |  3,809.468 ns | 54.6875 | 26.3672 |       - |  930152 B |
|                               |                    |               |              |               |         |         |         |           |
| NanoJson_JsonSpan             | ParseToSingleValue |  33,270.63 ns |   104.280 ns |     97.544 ns |       - |       - |       - |      56 B |
| NanoJson_JsonMemory + Dispose | ParseToSingleValue | 124,655.71 ns |   122.467 ns |     95.614 ns |       - |       - |       - |    3512 B |
| NanoJson_JsonMemory + GC      | ParseToSingleValue | 201,312.24 ns | 1,583.366 ns |  1,403.613 ns |  2.1973 |  1.9531 |       - |   39071 B |
| SystemTextJson_Node           | ParseToSingleValue | 138,104.68 ns |   708.213 ns |    591.390 ns |  5.8594 |  0.9766 |       - |  100032 B |
| SystemTextJson_Document       | ParseToSingleValue | 125,934.70 ns |   223.815 ns |    174.740 ns |       - |       - |       - |     128 B |
| LightJson                     | ParseToSingleValue | 334,775.95 ns | 3,829.818 ns |  3,395.034 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson                | ParseToSingleValue | 448,613.21 ns | 1,806.858 ns |  1,690.136 ns | 36.6211 | 26.3672 |       - |  618136 B |

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

## IDisposable Support
As JsonMemory uses arrays to store the data, it implements IDisposable to allow you to dispose of the arrays when they are no longer needed.
The pooled arrays are provided by NanoJsonStatics.JsonContainerPool, on dispose the arrays are returned to the pool for reuse.
```CS
private void Function(string jsonData) {
    using (JsonMemory JMemory = JsonMemory.ParseJson(jsonData)) {
        ... // Use JMemory here
    }

    JsonMemory JMemory2 = JsonMemory.ParseJson(jsonData);
    JMemory2.Dispose(); // Dispose of the arrays when no longer needed at your own leisure
}
```
This is not required as the provided arrays are wrapped inside a container class the returns them with a finaliser, however disposing them manually retains the speed benefits.

## JsonMemory arrays
JsonMemory objects and arrays are built using standard arrays, create the objects you want into a container and then use an Extension or static method for creating a Object or an Array with the array.

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