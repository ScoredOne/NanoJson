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

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6575/22H2/2022Update) <br>
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores <br>
.NET SDK 9.0.307 <br>
  [Host]   : .NET 6.0.36 (6.0.3624.51421), X64 RyuJIT AVX2 [AttachedDebugger] <br>
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 <br>

Job=.NET 9.0  Runtime=.NET 9.0 <br>

| Method         | format             | Mean      | Error    | StdDev   | Gen0    | Gen1    | Gen2    | Allocated |
|--------------- |------------------- |----------:|---------:|---------:|--------:|--------:|--------:|----------:|
| nJson*         | JustParse          |        NA |       NA |       NA |      NA |      NA |      NA |        NA |
| NanoJson       | JustParse          | 268.33 us | 3.342 us | 3.126 us |  4.3945 |  0.9766 |       - |   74928 B |
| SystemTextJson | JustParse          | 134.06 us | 0.558 us | 0.494 us |  5.6152 |  0.4883 |       - |   95720 B |
| LightJson      | JustParse          | 319.88 us | 1.975 us | 1.847 us | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | JustParse          | 450.08 us | 1.891 us | 1.676 us | 36.6211 | 24.4141 |       - |  616064 B |
| nJson*         | ParseToString      |        NA |       NA |       NA |      NA |      NA |      NA |        NA |
| NanoJson       | ParseToString      | 364.38 us | 4.406 us | 4.121 us | 30.2734 | 30.2734 | 30.2734 |  171146 B |
| SystemTextJson | ParseToString      | 250.93 us | 0.849 us | 0.753 us | 27.3438 | 27.3438 | 27.3438 |  183732 B |
| LightJson      | ParseToString      | 478.63 us | 0.997 us | 0.778 us | 42.4805 | 20.9961 |       - |  714936 B |
| NewtonsoftJson | ParseToString      | 635.58 us | 2.813 us | 2.631 us | 54.6875 | 53.7109 | 27.3438 |  801977 B |
| nJson          | ParseReformat      | 396.39 us | 1.381 us | 1.291 us |  5.8594 |  0.4883 |       - |  105064 B |
| NanoJson       | ParseReformat      | 344.40 us | 6.458 us | 6.041 us | 10.7422 |       - |       - |  179992 B |
| SystemTextJson | ParseReformat      | 314.37 us | 2.806 us | 2.625 us | 24.4141 | 10.7422 |       - |  415688 B |
| LightJson      | ParseReformat      | 434.00 us | 1.602 us | 1.499 us | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson | ParseReformat      | 681.19 us | 3.912 us | 3.659 us | 54.6875 | 41.0156 |       - |  928080 B |
| nJson          | ParseToSingleValue |  31.71 us | 0.066 us | 0.061 us |       - |       - |       - |     112 B |
| NanoJson       | ParseToSingleValue | 294.47 us | 3.777 us | 3.533 us |  4.3945 |  0.4883 |       - |   74984 B |
| SystemTextJson | ParseToSingleValue | 135.75 us | 0.877 us | 0.820 us |  5.8594 |  0.9766 |       - |   99992 B |
| LightJson      | ParseToSingleValue | 332.11 us | 2.682 us | 2.508 us | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson | ParseToSingleValue | 460.79 us | 1.790 us | 1.674 us | 36.6211 | 24.4141 |       - |  616064 B |
<br>
* nJson doesnt parse on data insersion hence the NA for JustParse and ParseToString, others were included given the search required to aquire value(s) <br><br>

test format:<br>
JustParse			= Create object of library and parse json file into it. No output requested from object.<br>
ParseToString		= Create object of library and parse json file into it. Take created object and request string of object.<br>
ParseReformat		= Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object.<br>
ParseToSingleValue	= Create object of library and parse json file into it. Receives index and string path and aquires a single value <br>

<br>
#1 NanoJson
<br>
Minimal allocation without compromise on time.<br>
Access constructors via static NanoJson.ParseJson ext.<br>
Object and Array types both accessible via indexer, dictionary is not implemented.<br>
Object and Array types both accessible via foreach, IEnumerator not supported, uses custom Enumerator<br>
Object type can also be accessed via key search. Furthermore, you can include a seperator (for example 'object.value') in your string and it will aquire the internal value.<br>
<br>
<br>
#1 nJson
<br>
The absolute smallest, parse on demand Json<br>
ref struct for methods quick access json data and extract what you need.<br>
Index support not included, intended for direct access for specific variables<br>
Foreach support included just like NanoJson, however speed is a bit slower than NanoJson due to parse on demand nature<br>