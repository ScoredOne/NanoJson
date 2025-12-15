# NanoJson

<br>NetStandard 2.1<br>

<br>Small and simple Json Parser to minimise memory allocation of Json.<br>
Theory;<br>
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily such as additional string allocations (Arrays cannot be avoided (so far))

<br>Nuget coming soon <br>

 <br>
// * Summary * <br>
 <br>
BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6691/22H2/2022Update) <br>
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores <br>
.NET SDK 9.0.307 <br>
  [Host]   : .NET 6.0.36 (6.0.3624.51421), X64 RyuJIT AVX2 [AttachedDebugger] <br>
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 <br>

Job=.NET 9.0  Runtime=.NET 9.0 <br>

| Method         | format             | Mean          | Error        | StdDev       | Gen0    | Gen1    | Gen2    | Allocated |
|--------------- |------------------- |--------------:|-------------:|-------------:|--------:|--------:|--------:|----------:|
| nJson*         | JustParse          |      34.96 ns |     0.253 ns |     0.237 ns |       - |       - |       - |         - |
| NanoJson       | JustParse          | 257,401.51 ns | 4,798.503 ns | 4,488.523 ns |  4.3945 |  0.9766 |       - |   74928 B |
| SystemTextJson | JustParse          | 135,840.36 ns | 1,407.106 ns | 1,316.208 ns |  5.6152 |  0.4883 |       - |   95720 B |
| LightJson      | JustParse          | 332,915.28 ns | 6,645.715 ns | 6,216.406 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | JustParse          | 490,189.47 ns | 8,933.286 ns | 8,356.202 ns | 36.1328 | 23.4375 |       - |  616064 B |
|                |                    |               |              |              |         |         |         |           |
| nJson*         | ParseToString      | 116,087.74 ns | 2,220.724 ns | 2,077.266 ns | 35.6445 | 35.6445 | 35.6445 |  114204 B |
| NanoJson       | ParseToString      | 448,532.13 ns | 6,183.289 ns | 5,481.324 ns | 30.2734 | 30.2734 | 30.2734 |  171146 B |
| SystemTextJson | ParseToString      | 252,498.30 ns | 2,280.401 ns | 2,021.516 ns | 27.3438 | 27.3438 | 27.3438 |  183732 B |
| LightJson      | ParseToString      | 495,948.28 ns | 7,350.923 ns | 6,516.402 ns | 42.4805 | 20.9961 |       - |  714936 B |
| NewtonsoftJson | ParseToString      | 631,081.54 ns | 7,874.481 ns | 6,980.522 ns | 54.6875 | 53.7109 | 27.3438 |  801977 B |
|                |                    |               |              |              |         |         |         |           |
| nJson          | ParseReformat      | 415,652.83 ns | 3,881.399 ns | 3,241.144 ns |  5.8594 |  0.4883 |       - |  105064 B |
| NanoJson       | ParseReformat      | 362,175.70 ns | 2,553.769 ns | 2,388.797 ns | 10.7422 |       - |       - |  179992 B |
| SystemTextJson | ParseReformat      | 320,474.14 ns | 6,093.533 ns | 6,772.946 ns | 23.4375 |  9.7656 |       - |  415688 B |
| LightJson      | ParseReformat      | 453,788.29 ns | 6,940.725 ns | 6,152.772 ns | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson | ParseReformat      | 651,335.42 ns | 8,306.892 ns | 7,770.272 ns | 54.6875 | 41.0156 |       - |  928080 B |
|                |                    |               |              |              |         |         |         |           |
| nJson          | ParseToSingleValue |  34,791.47 ns |   265.735 ns |   248.568 ns |       - |       - |       - |      56 B |
| NanoJson       | ParseToSingleValue | 254,382.96 ns | 3,170.271 ns | 2,810.363 ns |  4.3945 |  0.4883 |       - |   74984 B |
| SystemTextJson | ParseToSingleValue | 138,601.18 ns | 1,357.285 ns | 1,269.605 ns |  5.8594 |  0.9766 |       - |   99992 B |
| LightJson      | ParseToSingleValue | 339,216.17 ns | 5,939.660 ns | 5,555.962 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | ParseToSingleValue | 463,301.21 ns | 4,649.561 ns | 4,121.714 ns | 36.6211 | 24.4141 |       - |  616064 B |

*nJson doesnt parse on data insersion, JustParse is the speed for the class to be ready without parsing, ParseToString takes the received data and Decodes it without structure read or parsing. <br><br>

Test format:<br>
JustParse			= Create object of library and parse json file into it. No output requested from object.<br>
ParseToString		= Create object of library and parse json file into it. Take created object and request string of object.<br>
ParseReformat		= Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object.<br>
ParseToSingleValue	= Create object of library and parse json file into it. Receives index and string path and aquires a single value <br>
<br>

## NanoJson
Minimal allocation without compromise on time.<br>
Access constructors via static NanoJson.ParseJson ext.<br>
Object and Array types both accessible via indexer, dictionary is not implemented.<br>
Object and Array types both accessible via foreach, IEnumerator not supported, uses custom Enumerator<br>
Object type can also be accessed via key search. Furthermore, you can include a seperator (for example 'object.value') in your string and it will aquire the internal value.<br>
<br>

### Creating NanoJson Types
```CS
public static NanoJson ParseJson(string key, string data); // Translate string data into NanoJson
public static NanoJson ParseJson(string data);

public static NanoJson CreateArray(string key, NanoJson[] data); // Add Existing values to a Array
public static NanoJson CreateArray(NanoJson[] data);

public static NanoJson CreateObject(string key, NanoJson[] data); // Add Existing values to a Object
public static NanoJson CreateObject(NanoJson[] data);

public static NanoJson CreateStringObject(string key, string data);

public static NanoJson CreateBoolObject(string key, bool data);

public static NanoJson CreateNumberObject(string key, double data);

public static NanoJson ContainValueInObject(string key, NanoJson data); // To create a new NanoJson value with a new name
```
Statics are provided to create the objects you want. Constructors are private to maintain consistent object construction due to recursive loops.
<br>

## nJson
The absolute smallest, parse on demand Json<br>
ref struct for methods quick access json data and extract what you need.<br>
Index support not included, intended for direct access for specific variables<br>
Foreach support included just like NanoJson, however speed is a bit slower than NanoJson due to parse on demand nature<br>
<br>

### Creating nJson Types
```CS
private void Function(string jsonData) {
    nJson js = new nJson(jsonData);
}
```
With on demand parsing, the reference target goes in and it will use it later.
<br>