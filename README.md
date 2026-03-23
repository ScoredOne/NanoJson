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

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6937/22H2/2022Update)   
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores   
.NET SDK 9.0.307   
  [Host]   : .NET 5.0.17 (5.0.1722.21314), X64 RyuJIT AVX2 [AttachedDebugger]   
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 <br>

Job=.NET 9.0  Runtime=.NET 9.0

| Method                   | format             | Mean          | Error        | StdDev       | Gen0    | Gen1    | Gen2    | Allocated |
|------------------------- |------------------- |--------------:|-------------:|-------------:|--------:|--------:|--------:|----------:|
| NanoJson_JsonSpan*       | JustParse          |      32.24 ns |     0.010 ns |     0.008 ns |       - |       - |       - |         - |
| NanoJson_JsonMemory      | JustParse          | 111,541.05 ns |   499.438 ns |   417.053 ns |  4.3945 |  0.4883 |       - |   74928 B |
| SystemTextJson           | JustParse          | 137,323.25 ns |   496.235 ns |   464.179 ns |  5.6152 |  0.4883 |       - |   95696 B |
| LightJson                | JustParse          | 333,096.58 ns | 3,251.092 ns | 3,041.074 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson           | JustParse          | 453,760.88 ns | 3,189.760 ns | 2,983.704 ns | 36.6211 | 26.3672 |       - |  618136 B |
|                          |                    |               |              |              |         |         |         |           |
| NanoJson_JsonSpan        | ParseToString      | 468,709.76 ns |   968.156 ns |   858.246 ns | 30.2734 | 30.2734 | 30.2734 |   95650 B |
| NanoJson_JsonMemory      | ParseToString      | 242,663.77 ns | 2,748.266 ns | 2,436.266 ns | 30.2734 | 30.2734 | 30.2734 |  170599 B |
| SystemTextJson           | ParseToString      | 255,308.27 ns | 4,127.349 ns | 3,658.787 ns | 27.3438 | 27.3438 | 27.3438 |  183708 B |
| LightJson                | ParseToString      | 502,897.70 ns | 4,430.701 ns | 4,144.481 ns | 41.9922 | 20.5078 |       - |  714936 B |
| NewtonsoftJson           | ParseToString      | 626,962.83 ns | 7,376.277 ns | 6,899.774 ns | 54.6875 | 53.7109 | 27.3438 |  804049 B |
|                          |                    |               |              |              |         |         |         |           |
| NanoJson_JsonSpan**      | ParseReformat      | 401,155.84 ns | 2,732.017 ns | 2,421.861 ns |  5.8594 |  0.4883 |       - |  105744 B |
| NanoJson_JsonMemory      | ParseReformat      | 183,844.80 ns | 1,216.908 ns | 1,138.296 ns |  9.7656 |  1.2207 |       - |  166688 B |
| SystemTextJson           | ParseReformat      | 315,704.06 ns | 3,581.738 ns | 2,990.913 ns | 23.4375 |  9.7656 |       - |  415664 B |
| LightJson                | ParseReformat      | 471,427.29 ns | 5,288.662 ns | 4,947.018 ns | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson           | ParseReformat      | 668,827.49 ns | 6,130.534 ns | 5,734.506 ns | 54.6875 | 26.3672 |       - |  930152 B |
|                          |                    |               |              |              |         |         |         |           |
| NanoJson_JsonSpan        | ParseToSingleValue |  43,336.24 ns |    61.495 ns |    54.514 ns |       - |       - |       - |      56 B |
| NanoJson_JsonMemory      | ParseToSingleValue | 113,700.75 ns |   779.726 ns |   729.356 ns |  4.3945 |  0.4883 |       - |   74984 B |
| SystemTextJson           | ParseToSingleValue | 142,864.35 ns |   613.395 ns |   543.758 ns |  5.8594 |  0.9766 |       - |   99968 B |
| LightJson                | ParseToSingleValue | 348,184.74 ns | 1,754.150 ns | 1,464.795 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson           | ParseToSingleValue | 507,002.10 ns | 5,379.898 ns | 5,032.360 ns | 36.1328 | 25.3906 |       - |  618136 B |


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