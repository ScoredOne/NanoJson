# NanoJson

[![Static Badge](https://img.shields.io/badge/Nuget-Latest-teal?logo=nuget)](https://www.nuget.org/packages/NanoJson)
[![Static Badge](https://img.shields.io/badge/Github-repo-blue?logo=github)](https://github.com/ScoredOne/NanoJson)

NetStandard 2.1  

Small and simple Json Parser to minimise memory allocation of Json.  

Theory:
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily such as additional string allocations (Arrays cannot be avoided (so far))

// * Summary *

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6691/22H2/2022Update)  
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores  
.NET SDK 9.0.307  
  [Host]   : .NET 6.0.36 (6.0.3624.51421), X64 RyuJIT AVX2 [AttachedDebugger]  
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 <br>

Job=.NET 9.0  Runtime=.NET 9.0

| Method         | format             | Mean          | Error        | StdDev       | Gen0    | Gen1    | Gen2    | Allocated |
|--------------- |------------------- |--------------:|-------------:|-------------:|--------:|--------:|--------:|----------:|
| nJson*         | JustParse          |      43.43 ns |     0.053 ns |     0.049 ns |       - |       - |       - |         - |
| NJson          | JustParse          | 251,087.78 ns | 2,679.245 ns | 2,506.168 ns |  3.4180 |  0.4883 |       - |   61920 B |
| SystemTextJson | JustParse          | 135,782.57 ns |   484.123 ns |   429.162 ns |  5.6152 |  0.4883 |       - |   95720 B |
| LightJson      | JustParse          | 346,983.09 ns | 4,893.522 ns | 4,577.404 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | JustParse          | 486,106.12 ns | 2,195.694 ns | 1,946.425 ns | 36.6211 | 24.4141 |       - |  616064 B |
|                |                    |               |              |              |         |         |         |           |
| nJson*         | ParseToString      | 117,350.99 ns |   944.409 ns |   837.194 ns | 35.6445 | 35.6445 | 35.6445 |  114204 B |
| NJson          | ParseToString      | 392,163.70 ns | 2,750.329 ns | 2,438.095 ns |  8.3008 |  0.9766 |       - |  141552 B |
| SystemTextJson | ParseToString      | 247,326.05 ns | 1,885.486 ns | 1,671.435 ns | 27.3438 | 27.3438 | 27.3438 |  183732 B |
| LightJson      | ParseToString      | 485,330.54 ns | 1,673.183 ns | 1,483.233 ns | 42.4805 | 20.9961 |       - |  714936 B |
| NewtonsoftJson | ParseToString      | 632,642.24 ns | 5,754.675 ns | 4,805.414 ns | 54.6875 | 53.7109 | 27.3438 |  801977 B |
|                |                    |               |              |              |         |         |         |           |
| nJson          | ParseReformat      | 407,302.32 ns | 1,978.304 ns | 1,850.507 ns |  5.8594 |  0.4883 |       - |  105744 B |
| NJson          | ParseReformat      | 343,789.45 ns | 1,367.500 ns | 1,212.253 ns |  8.7891 |       - |       - |  153792 B |
| SystemTextJson | ParseReformat      | 317,485.20 ns | 1,923.727 ns | 1,799.455 ns | 24.4141 | 10.7422 |       - |  415688 B |
| LightJson      | ParseReformat      | 441,233.96 ns | 2,764.007 ns | 2,585.454 ns | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson | ParseReformat      | 672,066.93 ns | 7,962.808 ns | 7,448.415 ns | 54.6875 | 41.0156 |       - |  928080 B |
|                |                    |               |              |              |         |         |         |           |
| nJson          | ParseToSingleValue |  42,937.11 ns |    93.032 ns |    87.022 ns |       - |       - |       - |      56 B |
| NJson          | ParseToSingleValue | 251,638.21 ns | 3,553.146 ns | 3,323.615 ns |  3.4180 |  0.4883 |       - |   61976 B |
| SystemTextJson | ParseToSingleValue | 138,250.64 ns |   653.268 ns |   611.067 ns |  5.8594 |  0.9766 |       - |   99992 B |
| LightJson      | ParseToSingleValue | 326,678.29 ns | 2,247.122 ns | 2,101.960 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | ParseToSingleValue | 557,788.74 ns | 4,172.266 ns | 3,902.740 ns | 36.1328 | 23.4375 |       - |  616064 B |

*nJson doesnt parse on data insersion, JustParse is the speed for the class to be ready without parsing, ParseToString takes the received data and Decodes it without structure read or parsing.  

Test format:  
JustParse			= Create object of library and parse json file into it. No output requested from object.  
ParseToString		= Create object of library and parse json file into it. Take created object and request string of object.  
ParseReformat		= Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object.  
ParseToSingleValue	= Create object of library and parse json file into it. Receives index and string path and aquires a single value.  

## NJson
Minimal allocation without compromise on time.  
Access constructors via static NJson.ParseJson ext.  
Object and Array types both accessible via indexer, dictionary is not implemented.  
Object and Array types both accessible via foreach, IEnumerator not supported, uses custom Enumerator.  
Object type can also be accessed via key search. Furthermore, you can include a seperator (for example 'object.value') in your string and it will aquire the internal value.  

### Creating NJson Types
```CS
public static NJson ParseJson(string key, string data); // Translate string data into NJson
public static NJson ParseJson(string data);

public static NJson CreateArray(string key, NJson[] data); // Add Existing values to a Array
public static NJson CreateArray(NJson[] data);

public static NJson CreateObject(string key, NJson[] data); // Add Existing values to a Object
public static NJson CreateObject(NJson[] data);

public static NJson CreateString(string key, string data);

public static NJson CreateBool(string key, bool data);

public static NJson CreateNumber(string key, double data);

public static NJson AssignKeyToValue(string key, NJson data); // To create a new NJson value using existing data and a new name
```
Statics are provided to create the objects you want. Constructors are private to maintain consistent object construction due to recursive loops.  

## nJson
The absolute smallest, parse on demand Json.  
ref struct for methods quick access json data and extract what you need.  
Index support not included, intended for direct access for specific variables.  
Foreach support included just like NJson, however speed is a bit slower than NJson due to parse on demand nature.  

### Creating nJson Types
```CS
private void Function(string jsonData) {
    nJson js = new nJson(jsonData);
}
```
With on demand parsing, the reference target goes in and it will use it later.  

## Value Access  

### Foreach  

Both NJson and nJson support foreach iteration with a custom Enumerator, ref struct based for no allocation looping.  
Both Array and Objects can be looped or indexed to aquire its inner values. As a dictionary isnt used, larger scoped Objects will be slower to access than traditional hash code methods but on small scale objects the speed is negligable.    
```CS
private void Foreach(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    nJson nJ = new nJson(jsonData);
    foreach (NJson nano in NanoJ) {
        ...
    }
    foreach (nJson n in nJ) {
        ...
    }
}
```  

### Key Index 

Both NJson and nJson support key searching and key path searching for Json Objects (e.g Object -> Object -> Object -> Value).  

```CS
private void Key(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    nJson nJ = new nJson(jsonData);

    NJson NanoJValue1 = NanoJ["name"];
    NJson NanoJValue2 = NanoJ["name.name.value"]; // The same as NanoJ["name"]["name"]["value"]
    NJson NanoJValue3 = NanoJ["name.name"][2]["value"]; // The same as NanoJ["name"]["name"][2]["value"]

    nJson nJValue1 = nJ["name"];
    nJson nJValue2 = nJ["name.name.value"]; // The same as nJ["name"]["name"]["value"]
    nJson nJValue3 = nJ["name.name"][2]["value"]; // The same as nJ["name"]["name"][2]["value"]
}
``` 

### Numeric Index  

NJson as it uses arrays allows indexing support, this means index support is available for both Json Objects and Arrays.

```CS
private void Index(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    NJson NJ = Nanoj[0];

    nJson nJ = new nJson(jsonData);
    nJson nJIndex = nJ[0];
}
```  

nJson has index support however it should only be used to access a single value from the data, otherwise using the Enumerator is recommended.
The Enumerator stores the index and read position while directly asking for the index in a nJson object will not be remembered and will need to start the search from scratch.
For similar functionality with nJson, the Enumerator has been expanded and included a TryGetIndex method.  

```CS
private void Index(string jsonData) {
    nJson nJ = new nJson(jsonData);
    nJson.Enumerator nJ_Enum = nJ.GetEnumerator();
    if (nJ_Enum.TryGetIndex(2, out nJson nJ2)) {
        ...
    }
    // From here, nJ_Enum is at index 2, going backwards calls Reset() internally, going forward continues where it left off at 2
}
```  

No arrays are used in nJson so compromises must be made and the indexer was one of them.

## Extracting Json

### ToString

As NJson is a constructed format, ToString has been implemented to provide the current structure as a String.  
(Although sounding simple, remember no additional allocation)  
The body and sub values are evaluated to determine the character total required, rents the space and writes to it. Then gives the result to new String to create the output.  
Resulting in a body direct to string implimentation, no streams or string builders.  
```CS
private void GetString(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    string jsonString = NanoJ.ToString();
}
```  
An enum is also provided for additional settings. Using ToStringFormat you can combine options for ToString.  
```CS
private void GetString(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    string jsonString = NanoJ.ToString(ToStringFormat.Pretty | ToStringFormat.Decoded);

    // NanoJ.ToString() == NanoJ.ToString(ToStringFormat.All)
}
```  
Because of nJson's on demand nature, ToString functionality is not currently available, it will need to be pinned to extract it. (Future feature consideration)  

### TryGet*

Indexers are designed to throw, values that you are absolutely certain exist are aquired through indexers.  
To allow null returns on attempts, Try methods are provided. When failing to aquire a value, it will return the static Empty value (As structs cant be null).  
Provide the key just like the indexer.  

```CS
private void TryGet(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    nJson nJ = new nJson(jsonData);

    if (NanoJ.TryGetKey("name.name.value", out NJson value)) {
        ...
    } else {
        ... // value == NJson.Empty
    }

    if (nJ.TryGetKey("name.name.value", out nJson value)) {
        ...
    } else {
        ... // value == nJson.Empty
    }
}
``` 

### Creating new Json Strings

Using the static methods you can piece together new Json's

```CS
Console.WriteLine(
    new NJson[] {
		NJson.CreateBool("BoolKey", true),
		NJson.CreateNumber("NumberKey", 84526),
		NJson.CreateNull("NullKey"),
		NJson.CreateDateTime("DateTimeKey", DateTime.Now),
		NJson.CreateString("StringKey", "Test String Words"),
		NJson.CreateArray("ArrayKey", new NJson[] {
			NJson.CreateString(null, "Array 1"),
			NJson.CreateBool(null, true),
			NJson.CreateDateTime(null, DateTime.MaxValue),
			NJson.CreateNull(null),
			NJson.CreateNumber(null, 4892),
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