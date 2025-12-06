# NanoJson

<br>NetStandard 2.1<br>

<br>Small and simple Json Parser to minimise memory allocation of Json.<br>
Theory;<br>
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily such as additional string allocations (Arrays cannot be avoided (so far))

<br>Nuget coming soon <br>

// * Summary *

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6575/22H2/2022Update)<br>
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores<br>
.NET SDK 9.0.307<br>
  [Host]   : .NET 6.0.36 (6.0.3624.51421), X64 RyuJIT AVX2 [AttachedDebugger]<br>
  .NET 6.0 : .NET 6.0.36 (6.0.3624.51421), X64 RyuJIT AVX2<br>
  .NET 7.0 : .NET 7.0.20 (7.0.2024.26716), X64 RyuJIT AVX2<br>
  .NET 8.0 : .NET 8.0.22 (8.0.2225.52707), X64 RyuJIT AVX2<br>
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2<br><br>


| Method         | Runtime  | format        | Mean       | Error   | StdDev  | Gen0    | Gen1    | Gen2    | Allocated |
|--------------- | -------- |-------------- |-----------:|--------:|--------:|--------:|--------:|--------:|----------:|
| NanoJson       | .NET 6.0 | JustParse     |   338.8 us | 3.15 us | 2.80 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 6.0 | JustParse     |   184.4 us | 0.42 us | 0.39 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 6.0 | JustParse     |   607.3 us | 8.76 us | 8.19 us | 32.2266 | 10.7422 |       - | 529.73 KB |
| NewtonsoftJson | .NET 6.0 | JustParse     |   703.9 us | 3.73 us | 3.49 us | 36.1328 | 16.6016 |       - | 601.63 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 7.0 | JustParse     |   302.8 us | 2.46 us | 2.05 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 7.0 | JustParse     |   183.2 us | 0.75 us | 0.71 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 7.0 | JustParse     |   573.8 us | 3.90 us | 3.65 us | 32.2266 | 13.6719 |       - | 529.73 KB |
| NewtonsoftJson | .NET 7.0 | JustParse     |   648.3 us | 2.43 us | 2.15 us | 36.1328 | 23.4375 |       - | 601.63 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 8.0 | JustParse     |   287.3 us | 2.97 us | 2.78 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 8.0 | JustParse     |   136.5 us | 1.01 us | 0.94 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 8.0 | JustParse     |   338.2 us | 4.16 us | 3.89 us | 32.2266 | 14.1602 |       - | 529.73 KB |
| NewtonsoftJson | .NET 8.0 | JustParse     |   484.6 us | 4.36 us | 4.08 us | 36.6211 | 24.4141 |       - | 601.63 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 9.0 | JustParse     |   281.5 us | 2.04 us | 1.70 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 9.0 | JustParse     |   136.7 us | 0.73 us | 0.68 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 9.0 | JustParse     |   324.8 us | 1.76 us | 1.65 us | 32.2266 | 14.1602 |       - | 529.73 KB |
| NewtonsoftJson | .NET 9.0 | JustParse     |   459.4 us | 3.05 us | 2.85 us | 36.6211 | 24.4141 |       - | 601.63 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 6.0 | ParseToString |   467.3 us | 3.37 us | 2.99 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 6.0 | ParseToString |   320.0 us | 1.60 us | 1.42 us | 27.3438 | 27.3438 | 27.3438 | 179.56 KB |
| LightJson      | .NET 6.0 | ParseToString |   884.9 us | 4.67 us | 4.37 us | 42.9688 | 12.6953 |       - |  706.1 KB |
| NewtonsoftJson | .NET 6.0 | ParseToString |   923.3 us | 5.96 us | 5.29 us | 54.6875 | 27.3438 | 27.3438 | 783.18 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 7.0 | ParseToString |   406.1 us | 6.67 us | 6.24 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 7.0 | ParseToString |   312.7 us | 2.41 us | 2.25 us | 27.3438 | 27.3438 | 27.3438 | 179.56 KB |
| LightJson      | .NET 7.0 | ParseToString |   826.8 us | 3.74 us | 3.50 us | 42.9688 | 17.5781 |       - |  706.1 KB |
| NewtonsoftJson | .NET 7.0 | ParseToString |   950.3 us | 4.26 us | 3.99 us | 54.6875 | 53.7109 | 27.3438 | 783.18 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 8.0 | ParseToString |   400.8 us | 2.66 us | 2.49 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 8.0 | ParseToString |   254.9 us | 1.80 us | 1.69 us | 27.3438 | 27.3438 | 27.3438 | 179.56 KB |
| LightJson      | .NET 8.0 | ParseToString |   501.6 us | 6.58 us | 5.83 us | 41.9922 | 20.5078 |       - | 698.18 KB |
| NewtonsoftJson | .NET 8.0 | ParseToString |   664.4 us | 6.47 us | 6.06 us | 54.6875 | 53.7109 | 27.3438 | 783.18 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 9.0 | ParseToString |   432.8 us | 3.80 us | 3.55 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 9.0 | ParseToString |   253.3 us | 4.44 us | 4.15 us | 27.3438 | 27.3438 | 27.3438 | 179.43 KB |
| LightJson      | .NET 9.0 | ParseToString |   473.6 us | 3.47 us | 3.24 us | 42.4805 | 20.9961 |       - | 698.18 KB |
| NewtonsoftJson | .NET 9.0 | ParseToString |   609.4 us | 5.67 us | 5.03 us | 54.6875 | 53.7109 | 27.3438 | 783.18 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 6.0 | ParseReformat |   437.8 us | 4.42 us | 3.92 us | 10.2539 |  1.9531 |       - | 169.91 KB |
| SystemTextJson | .NET 6.0 | ParseReformat |   616.7 us | 2.68 us | 2.24 us | 31.2500 | 11.7188 |       - | 523.49 KB |
| LightJson      | .NET 6.0 | ParseReformat |   831.5 us | 5.22 us | 4.89 us | 42.9688 | 13.6719 |       - | 710.34 KB |
| NewtonsoftJson | .NET 6.0 | ParseReformat | 1,046.9 us | 7.26 us | 6.80 us | 54.6875 | 27.3438 |       - | 911.05 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 7.0 | ParseReformat |   405.6 us | 1.66 us | 1.30 us | 10.2539 |  1.9531 |       - | 169.91 KB |
| SystemTextJson | .NET 7.0 | ParseReformat |   583.1 us | 2.84 us | 2.37 us | 31.2500 | 18.5547 |       - | 523.49 KB |
| LightJson      | .NET 7.0 | ParseReformat |   766.6 us | 3.32 us | 2.95 us | 42.9688 |       - |       - | 710.34 KB |
| NewtonsoftJson | .NET 7.0 | ParseReformat |   942.7 us | 5.70 us | 5.33 us | 55.6641 | 44.9219 |       - | 911.05 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 8.0 | ParseReformat |   397.2 us | 7.01 us | 6.55 us | 10.2539 |  1.9531 |       - | 169.91 KB |
| SystemTextJson | .NET 8.0 | ParseReformat |   443.6 us | 3.55 us | 3.15 us | 31.7383 | 15.6250 |       - | 519.04 KB |
| LightJson      | .NET 8.0 | ParseReformat |   477.4 us | 4.36 us | 4.08 us | 43.4570 |       - |       - | 710.34 KB |
| NewtonsoftJson | .NET 8.0 | ParseReformat |   707.7 us | 5.68 us | 5.31 us | 55.6641 | 44.9219 |       - | 911.05 KB |
|                |          |               |            |         |         |         |         |         |           |
| NanoJson       | .NET 9.0 | ParseReformat |   379.5 us | 2.07 us | 1.83 us | 10.2539 |  1.9531 |       - | 169.91 KB |
| SystemTextJson | .NET 9.0 | ParseReformat |   353.3 us | 2.58 us | 2.16 us | 25.3906 | 15.6250 |       - | 437.17 KB |
| LightJson      | .NET 9.0 | ParseReformat |   439.0 us | 2.43 us | 2.03 us | 43.4570 |       - |       - | 710.34 KB |
| NewtonsoftJson | .NET 9.0 | ParseReformat |   665.7 us | 5.64 us | 5.00 us | 55.6641 | 44.9219 |       - | 911.05 KB |


format:<br>
JustParse        = Create object of library and parse json file into it. No output requested from object.<br>
ParseToString	 = Create object of library and parse json file into it. Take created object and request string of object.<br>
ParseReformat    = Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object.<br>
