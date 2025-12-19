# NanoJson

[![NuGet package](https://img.shields.io/nuget/v/Nerdbank.GitVersioning.svg)](https://www.nuget.org/packages/NJson)

NetStandard 2.1

Small and simple Json Parser to minimise memory allocation of Json.


Theory;
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
| nJson*         | JustParse          |      34.96 ns |     0.253 ns |     0.237 ns |       - |       - |       - |         - |
| NJson          | JustParse          | 257,401.51 ns | 4,798.503 ns | 4,488.523 ns |  4.3945 |  0.9766 |       - |   74928 B |
| SystemTextJson | JustParse          | 135,840.36 ns | 1,407.106 ns | 1,316.208 ns |  5.6152 |  0.4883 |       - |   95720 B |
| LightJson      | JustParse          | 332,915.28 ns | 6,645.715 ns | 6,216.406 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | JustParse          | 490,189.47 ns | 8,933.286 ns | 8,356.202 ns | 36.1328 | 23.4375 |       - |  616064 B |
|                |                    |               |              |              |         |         |         |           |
| nJson*         | ParseToString      | 116,087.74 ns | 2,220.724 ns | 2,077.266 ns | 35.6445 | 35.6445 | 35.6445 |  114204 B |
| NJson          | ParseToString      | 448,532.13 ns | 6,183.289 ns | 5,481.324 ns | 30.2734 | 30.2734 | 30.2734 |  171146 B |
| SystemTextJson | ParseToString      | 252,498.30 ns | 2,280.401 ns | 2,021.516 ns | 27.3438 | 27.3438 | 27.3438 |  183732 B |
| LightJson      | ParseToString      | 495,948.28 ns | 7,350.923 ns | 6,516.402 ns | 42.4805 | 20.9961 |       - |  714936 B |
| NewtonsoftJson | ParseToString      | 631,081.54 ns | 7,874.481 ns | 6,980.522 ns | 54.6875 | 53.7109 | 27.3438 |  801977 B |
|                |                    |               |              |              |         |         |         |           |
| nJson          | ParseReformat      | 415,652.83 ns | 3,881.399 ns | 3,241.144 ns |  5.8594 |  0.4883 |       - |  105064 B |
| NJson          | ParseReformat      | 362,175.70 ns | 2,553.769 ns | 2,388.797 ns | 10.7422 |       - |       - |  179992 B |
| SystemTextJson | ParseReformat      | 320,474.14 ns | 6,093.533 ns | 6,772.946 ns | 23.4375 |  9.7656 |       - |  415688 B |
| LightJson      | ParseReformat      | 453,788.29 ns | 6,940.725 ns | 6,152.772 ns | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson | ParseReformat      | 651,335.42 ns | 8,306.892 ns | 7,770.272 ns | 54.6875 | 41.0156 |       - |  928080 B |
|                |                    |               |              |              |         |         |         |           |
| nJson          | ParseToSingleValue |  34,791.47 ns |   265.735 ns |   248.568 ns |       - |       - |       - |      56 B |
| NJson          | ParseToSingleValue | 254,382.96 ns | 3,170.271 ns | 2,810.363 ns |  4.3945 |  0.4883 |       - |   74984 B |
| SystemTextJson | ParseToSingleValue | 138,601.18 ns | 1,357.285 ns | 1,269.605 ns |  5.8594 |  0.9766 |       - |   99992 B |
| LightJson      | ParseToSingleValue | 339,216.17 ns | 5,939.660 ns | 5,555.962 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | ParseToSingleValue | 463,301.21 ns | 4,649.561 ns | 4,121.714 ns | 36.6211 | 24.4141 |       - |  616064 B |

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

public static NJson CreateStringObject(string key, string data);

public static NJson CreateBoolObject(string key, bool data);

public static NJson CreateNumberObject(string key, double data);

public static NJson ContainValueInObject(string key, NJson data); // To create a new NJson value with a new name
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
The Enumerator stores the index and read position while directly asking for the index in a nJson object will not be rememebered and will need to start the search from scratch.
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

As NJson is a constructed format, ToString has been developed to provide the structure as a String.  
(Although sounding simple, remember no additional allocation)  
The body and sub values are evaluated to determin the character total required, rents the space and writes to it. The gives the result to String to create the output.  
```CS
private void GetString(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    string jsonString = NanoJ.ToString();
}
```  
A enum is also provided for additional settings.  
Via the provided enum ToStringFormat you can combine options for ToString.  
```CS
private void GetString(string jsonData) {
    NJson NanoJ = NJson.ParseJson(jsonData);
    string jsonString = NanoJ.ToString(ToStringFormat.Pretty | ToStringFormat.Decoded);

    // NanoJ.ToString() == NanoJ.ToString(ToStringFormat.All)
}
```  
Because of nJson's on demand nature, ToString functionality is not available, it will need to be pinned to extract it. (Future feature consideration)  

### TryGet*

Indexers are designed to throw, values that you are absolutely certain exist are aquired through indexers.  
To allow null returns on attemps, Try methods are provided. When failing to aquire a value, it will return the static Empty value (As structs cant be null).  
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
