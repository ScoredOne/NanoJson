# NanoJson

<br>NetStandard 2.1<br>

<br>Small and simple Json Parser to minimise memory allocation of Json.<br>
Theory;<br>
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily like arrays

<br>Nuget coming soon <br>

// * Summary *<br>

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6575/22H2/2022Update)<br>
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores<br>
.NET SDK 9.0.307<br>
  [Host]     : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2 [AttachedDebugger]<br>
  DefaultJob : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2<br>

// * Summary *

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.6575/22H2/2022Update)
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores
.NET SDK 9.0.307
  [Host]   : .NET 6.0.36 (6.0.3624.51421), X64 RyuJIT AVX2 [AttachedDebugger]
  .NET 6.0 : .NET 6.0.36 (6.0.3624.51421), X64 RyuJIT AVX2
  .NET 7.0 : .NET 7.0.20 (7.0.2024.26716), X64 RyuJIT AVX2
  .NET 8.0 : .NET 8.0.22 (8.0.2225.52707), X64 RyuJIT AVX2
  .NET 9.0 : .NET 9.0.11 (9.0.1125.51716), X64 RyuJIT AVX2


| Method         | Job      | Runtime  | format        | Mean       | Error   | StdDev  | Gen0    | Gen1    | Gen2    | Allocated |
|--------------- |--------- |--------- |-------------- |-----------:|--------:|--------:|--------:|--------:|--------:|----------:|
| NanoJson       | .NET 6.0 | .NET 6.0 | JustParse     |   358.4 us | 0.18 us | 0.14 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 6.0 | .NET 6.0 | JustParse     |   196.3 us | 1.08 us | 1.01 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 6.0 | .NET 6.0 | JustParse     |   633.1 us | 4.13 us | 3.86 us | 32.2266 | 10.7422 |       - | 529.73 KB |
| NewtonsoftJson | .NET 6.0 | .NET 6.0 | JustParse     |   709.9 us | 5.06 us | 4.74 us | 36.1328 | 16.6016 |       - | 601.63 KB |
| NanoJson       | .NET 7.0 | .NET 7.0 | JustParse     |   416.8 us | 2.94 us | 2.75 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 7.0 | .NET 7.0 | JustParse     |   185.0 us | 0.80 us | 0.67 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 7.0 | .NET 7.0 | JustParse     |   559.0 us | 3.95 us | 3.70 us | 32.2266 | 13.6719 |       - | 529.73 KB |
| NewtonsoftJson | .NET 7.0 | .NET 7.0 | JustParse     |   661.6 us | 5.27 us | 4.67 us | 36.1328 | 23.4375 |       - | 601.63 KB |
| NanoJson       | .NET 8.0 | .NET 8.0 | JustParse     |   296.4 us | 2.89 us | 2.71 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 8.0 | .NET 8.0 | JustParse     |   137.4 us | 0.68 us | 0.64 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 8.0 | .NET 8.0 | JustParse     |   337.0 us | 1.57 us | 1.46 us | 32.2266 | 14.1602 |       - | 529.73 KB |
| NewtonsoftJson | .NET 8.0 | .NET 8.0 | JustParse     |   511.6 us | 6.64 us | 6.21 us | 36.1328 | 23.4375 |       - | 601.63 KB |
| NanoJson       | .NET 9.0 | .NET 9.0 | JustParse     |   338.7 us | 2.23 us | 2.08 us |  4.3945 |  0.9766 |       - |  73.17 KB |
| SystemTextJson | .NET 9.0 | .NET 9.0 | JustParse     |   135.9 us | 1.19 us | 1.12 us |  5.6152 |  0.4883 |       - |  93.48 KB |
| LightJson      | .NET 9.0 | .NET 9.0 | JustParse     |   318.9 us | 1.68 us | 1.57 us | 32.2266 | 14.1602 |       - | 529.73 KB |
| NewtonsoftJson | .NET 9.0 | .NET 9.0 | JustParse     |   434.8 us | 2.45 us | 2.29 us | 36.6211 | 24.4141 |       - | 601.63 KB |
| NanoJson       | .NET 6.0 | .NET 6.0 | ParseToString |   469.4 us | 1.36 us | 1.13 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 6.0 | .NET 6.0 | ParseToString |   307.5 us | 0.99 us | 0.92 us | 27.3438 | 27.3438 | 27.3438 | 179.56 KB |
| LightJson      | .NET 6.0 | .NET 6.0 | ParseToString |   869.6 us | 7.07 us | 6.61 us | 42.9688 | 12.6953 |       - |  706.1 KB |
| NewtonsoftJson | .NET 6.0 | .NET 6.0 | ParseToString |   918.0 us | 5.48 us | 5.13 us | 54.6875 | 27.3438 | 27.3438 | 783.18 KB |
| NanoJson       | .NET 7.0 | .NET 7.0 | ParseToString |   531.4 us | 1.04 us | 0.98 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 7.0 | .NET 7.0 | ParseToString |   308.4 us | 0.62 us | 0.51 us | 27.3438 | 27.3438 | 27.3438 | 179.56 KB |
| LightJson      | .NET 7.0 | .NET 7.0 | ParseToString |   821.1 us | 2.67 us | 2.36 us | 42.9688 | 17.5781 |       - |  706.1 KB |
| NewtonsoftJson | .NET 7.0 | .NET 7.0 | ParseToString |   958.0 us | 3.31 us | 3.10 us | 54.6875 | 53.7109 | 27.3438 | 783.18 KB |
| NanoJson       | .NET 8.0 | .NET 8.0 | ParseToString |   427.1 us | 0.72 us | 0.64 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 8.0 | .NET 8.0 | ParseToString |   249.1 us | 1.01 us | 0.89 us | 27.3438 | 27.3438 | 27.3438 | 179.56 KB |
| LightJson      | .NET 8.0 | .NET 8.0 | ParseToString |   494.8 us | 3.20 us | 2.99 us | 41.9922 | 20.5078 |       - | 698.18 KB |
| NewtonsoftJson | .NET 8.0 | .NET 8.0 | ParseToString |   652.0 us | 2.72 us | 2.41 us | 54.6875 | 53.7109 | 27.3438 | 783.18 KB |
| NanoJson       | .NET 9.0 | .NET 9.0 | ParseToString |   418.6 us | 0.69 us | 0.62 us | 30.2734 | 30.2734 | 30.2734 | 167.13 KB |
| SystemTextJson | .NET 9.0 | .NET 9.0 | ParseToString |   246.5 us | 1.00 us | 0.94 us | 27.3438 | 27.3438 | 27.3438 | 179.43 KB |
| LightJson      | .NET 9.0 | .NET 9.0 | ParseToString |   481.6 us | 2.87 us | 2.69 us | 42.4805 | 20.9961 |       - | 698.18 KB |
| NewtonsoftJson | .NET 9.0 | .NET 9.0 | ParseToString |   623.9 us | 2.33 us | 2.07 us | 54.6875 | 53.7109 | 27.3438 | 783.18 KB |
| NanoJson       | .NET 6.0 | .NET 6.0 | ParseReformat |   452.7 us | 2.35 us | 2.20 us | 10.2539 |  1.9531 |       - | 169.91 KB |
| SystemTextJson | .NET 6.0 | .NET 6.0 | ParseReformat |   594.0 us | 1.70 us | 1.59 us | 31.2500 | 11.7188 |       - | 523.49 KB |
| LightJson      | .NET 6.0 | .NET 6.0 | ParseReformat |   826.2 us | 7.70 us | 7.20 us | 42.9688 | 13.6719 |       - | 710.34 KB |
| NewtonsoftJson | .NET 6.0 | .NET 6.0 | ParseReformat | 1,001.5 us | 4.98 us | 4.66 us | 54.6875 | 27.3438 |       - | 911.05 KB |
| NanoJson       | .NET 7.0 | .NET 7.0 | ParseReformat |   491.8 us | 0.85 us | 0.71 us |  9.7656 |  0.9766 |       - | 169.91 KB |
| SystemTextJson | .NET 7.0 | .NET 7.0 | ParseReformat |   570.3 us | 1.50 us | 1.25 us | 31.2500 | 18.5547 |       - | 523.49 KB |
| LightJson      | .NET 7.0 | .NET 7.0 | ParseReformat |   772.2 us | 3.58 us | 3.17 us | 42.9688 |       - |       - | 710.34 KB |
| NewtonsoftJson | .NET 7.0 | .NET 7.0 | ParseReformat | 1,019.7 us | 5.09 us | 4.76 us | 54.6875 | 44.9219 |       - | 911.05 KB |
| NanoJson       | .NET 8.0 | .NET 8.0 | ParseReformat |   390.2 us | 1.44 us | 1.34 us | 10.2539 |  1.9531 |       - | 169.91 KB |
| SystemTextJson | .NET 8.0 | .NET 8.0 | ParseReformat |   444.1 us | 2.13 us | 1.99 us | 31.7383 | 15.6250 |       - | 519.04 KB |
| LightJson      | .NET 8.0 | .NET 8.0 | ParseReformat |   471.2 us | 1.62 us | 1.44 us | 43.4570 |       - |       - | 710.34 KB |
| NewtonsoftJson | .NET 8.0 | .NET 8.0 | ParseReformat |   715.0 us | 3.33 us | 3.12 us | 55.6641 | 44.9219 |       - | 911.05 KB |
| NanoJson       | .NET 9.0 | .NET 9.0 | ParseReformat |   396.3 us | 0.73 us | 0.57 us | 10.2539 |  1.9531 |       - | 169.91 KB |
| SystemTextJson | .NET 9.0 | .NET 9.0 | ParseReformat |   349.0 us | 1.81 us | 1.51 us | 25.3906 | 15.6250 |       - | 437.17 KB |
| LightJson      | .NET 9.0 | .NET 9.0 | ParseReformat |   448.0 us | 1.58 us | 1.48 us | 43.4570 |       - |       - | 710.34 KB |
| NewtonsoftJson | .NET 9.0 | .NET 9.0 | ParseReformat |   640.7 us | 3.14 us | 2.62 us | 55.6641 | 44.9219 |       - | 911.05 KB |

format:<br>
JustParse        = Create object of library and parse json file into it. No output requested from object.<br>
ParseToString	 = Create object of library and parse json file into it. Take created object and request string of object.<br>
ParseReformat    = Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object.<br>
