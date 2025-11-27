# NanoJson

<br>NetStandard 2.1<br>

<br>Small and simple Json Parser to minimise memory allocation of Json.<br>
Theory;<br>
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily like arrays

<br>Nuget coming soon <br>
<br>TODO:
- Reformate ToString to pre determine the size needed for the string builder container to stop resize allocations occuring there.

<br>Net 9 Summary<br>
// * Summary *<br>

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6575/22H2/2022Update)<br>
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores<br>
.NET SDK 9.0.307<br>
  [Host]     : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 [AttachedDebugger]<br>
  DefaultJob : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2<br>


| Method         | format        | Mean     | Error   | StdDev  | Gen0    | Gen1    | Gen2    | Allocated |
|--------------- |-------------- |---------:|--------:|--------:|--------:|--------:|--------:|----------:|
| NanoJson       | JustParse     | 344.0 us | 0.72 us | 0.60 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | JustParse     | 134.7 us | 0.69 us | 0.64 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | JustParse     | 330.9 us | 2.82 us | 2.64 us | 32.2266 | 14.1602 |       - | 529.73 KB |
| NewtonsoftJson | JustParse     | 462.0 us | 2.90 us | 2.71 us | 36.6211 | 24.4141 |       - | 601.63 KB |
| NanoJson       | ParseToString | 421.5 us | 0.60 us | 0.53 us | 30.2734 | 30.2734 | 30.2734 | 262.31 KB |
| SystemTextJson | ParseToString | 246.6 us | 1.28 us | 1.20 us | 27.3438 | 27.3438 | 27.3438 | 179.43 KB |
| LightJson      | ParseToString | 485.9 us | 2.90 us | 2.57 us | 41.9922 | 20.5078 |       - | 698.18 KB |
| NewtonsoftJson | ParseToString | 616.7 us | 2.44 us | 2.29 us | 54.6875 | 53.7109 | 27.3438 | 783.18 KB |
| NanoJson       | ParseReformat | 394.8 us | 2.07 us | 1.73 us | 14.1602 |  2.4414 |       - |  233.7 KB |
| SystemTextJson | ParseReformat | 356.6 us | 2.57 us | 2.14 us | 25.3906 | 15.6250 |       - | 437.17 KB |
| LightJson      | ParseReformat | 460.4 us | 2.19 us | 1.83 us | 43.4570 |       - |       - | 710.34 KB |
| NewtonsoftJson | ParseReformat | 652.2 us | 4.20 us | 3.93 us | 55.6641 | 44.9219 |       - | 911.05 KB |

format:<br>
JustParse        = Create object of library and parse json file into it. No output requested from object.<br>
ParseToString   = Create object of library and parse json file into it. Take created object and request string of object.<br>
ParseReformat    = Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object.<br>
