# NanoJson

[![Static Badge](https://img.shields.io/badge/Nuget-Latest-teal?logo=nuget)](https://www.nuget.org/packages/NanoJson)
[![Static Badge](https://img.shields.io/badge/Github-repo-blue?logo=github)](https://github.com/ScoredOne/NanoJson)

NetStandard 2.1  

Small and simple Json Parser to minimise memory allocation of Json.  

Theory:
- String goes in
- Get Memory of String
- Translate memory area into key and value information not creating new regions or allocations unneccessarily such as additional string allocations.

// * Summary *

BenchmarkDotNet v0.15.2, Windows 10 (10.0.19045.7417/22H2/2022Update)
AMD Ryzen 9 5950X 3.50GHz, 1 CPU, 16 logical and 16 physical cores
.NET SDK 9.0.315
  [Host]           : .NET 5.0.17 (5.0.1722.21314), X64 RyuJIT AVX2 [AttachedDebugger]
  LongRun-.NET 9.0 : .NET 9.0.17 (9.0.1726.26416), X64 RyuJIT AVX2 <br>

Job=LongRun-.NET 9.0  Runtime=.NET 9.0  IterationCount=100
LaunchCount=3  WarmupCount=15

| Method                        | format             | Mean         | Error       | StdDev       | Median       | Gen0    | Gen1    | Gen2    | Allocated |
|------------------------------ |------------------- |-------------:|------------:|-------------:|-------------:|--------:|--------:|--------:|----------:|
| NanoJson_JsonSpan*            | JustParse          |     106.8 ns |     0.12 ns |      0.63 ns |     107.1 ns |       - |       - |       - |         - |
| NanoJson_JsonMemory + Dispose | JustParse          | 134,805.6 ns |   819.58 ns |  4,227.63 ns | 135,142.9 ns |       - |       - |       - |         - |
| NanoJson_JsonMemory + GC      | JustParse          | 138,719.7 ns |   373.84 ns |  1,941.74 ns | 138,304.5 ns |  6.3477 |  1.2207 |       - |  109968 B |
| SystemTextJson_Node           | JustParse          | 137,969.8 ns |   245.11 ns |  1,273.07 ns | 137,712.1 ns |  5.6152 |  0.9766 |       - |   95728 B |
| SystemTextJson_Document       | JustParse          | 128,437.8 ns |   240.57 ns |  1,253.77 ns | 128,408.8 ns |       - |       - |       - |      72 B |
| LightJson                     | JustParse          | 343,159.7 ns |   990.26 ns |  5,134.61 ns | 342,950.3 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson                | JustParse          | 468,055.1 ns | 1,618.99 ns |  8,437.78 ns | 468,527.7 ns | 36.6211 | 26.3672 |       - |  618136 B |
|                               |                    |              |             |              |              |         |         |         |           |
| NanoJson_JsonSpan             | ParseToString      | 399,480.7 ns | 3,461.44 ns | 18,009.44 ns | 390,467.6 ns | 28.3203 | 28.3203 | 28.3203 |   95649 B |
| NanoJson_JsonMemory + Dispose | ParseToString      | 265,354.2 ns |   755.25 ns |  3,929.47 ns | 265,416.4 ns | 30.2734 | 30.2734 | 30.2734 |   95650 B |
| NanoJson_JsonMemory + GC      | ParseToString      | 277,224.4 ns | 1,493.48 ns |  7,783.65 ns | 277,132.3 ns | 30.2734 | 30.2734 | 30.2734 |  205618 B |
| SystemTextJson_Node           | ParseToString      | 261,574.8 ns |   956.13 ns |  4,923.45 ns | 261,256.3 ns | 27.3438 | 27.3438 | 27.3438 |  183740 B |
| SystemTextJson_Document       | ParseToString      | 179,563.1 ns |   525.68 ns |  2,711.60 ns | 179,376.3 ns | 35.6445 | 35.6445 | 35.6445 |  114308 B |
| LightJson                     | ParseToString      | 512,072.5 ns | 1,390.94 ns |  7,236.89 ns | 511,445.7 ns | 41.9922 | 20.5078 |       - |  714936 B |
| NewtonsoftJson                | ParseToString      | 652,492.1 ns | 2,526.51 ns | 13,167.53 ns | 652,549.5 ns | 54.6875 | 53.7109 | 27.3438 |  804049 B |
|                               |                    |              |             |              |              |         |         |         |           |
| NanoJson_JsonSpan**           | ParseReformat      | 393,114.6 ns | 1,336.79 ns |  6,955.18 ns | 392,481.4 ns |  6.3477 |  0.9766 |       - |  112040 B |
| NanoJson_JsonMemory + Dispose | ParseReformat      | 221,980.0 ns |   697.31 ns |  3,634.21 ns | 222,179.3 ns |  5.6152 |  0.4883 |       - |   98056 B |
| NanoJson_JsonMemory + GC      | ParseReformat      | 217,106.6 ns |   684.99 ns |  3,569.98 ns | 217,497.4 ns | 12.2070 |  3.9063 |       - |  208024 B |
| SystemTextJson_Node           | ParseReformat      | 334,806.5 ns | 1,279.17 ns |  6,621.20 ns | 334,769.4 ns | 23.4375 |  9.7656 |       - |  415696 B |
| SystemTextJson_Document       | ParseReformat      | 249,975.5 ns |   541.02 ns |  2,800.41 ns | 249,713.1 ns |  7.8125 |       - |       - |  149864 B |
| LightJson                     | ParseReformat      | 479,086.8 ns | 1,416.03 ns |  7,354.85 ns | 477,931.2 ns | 42.9688 | 18.0664 |       - |  725168 B |
| NewtonsoftJson                | ParseReformat      | 671,121.2 ns | 5,678.09 ns | 29,592.74 ns | 684,653.5 ns | 54.6875 | 26.3672 |       - |  930152 B |
|                               |                    |              |             |              |              |         |         |         |           |
| NanoJson_JsonSpan             | ParseToSingleValue |  34,002.4 ns |   258.45 ns |  1,323.86 ns |  33,410.6 ns |       - |       - |       - |      56 B |
| NanoJson_JsonMemory + Dispose | ParseToSingleValue | 140,419.4 ns |   456.27 ns |  2,369.86 ns | 140,275.1 ns |       - |       - |       - |      56 B |
| NanoJson_JsonMemory + GC      | ParseToSingleValue | 138,948.5 ns |   578.82 ns |  3,011.53 ns | 138,568.5 ns |  6.3477 |  1.2207 |       - |  110024 B |
| SystemTextJson_Node           | ParseToSingleValue | 140,788.8 ns |   285.23 ns |  1,478.93 ns | 140,810.9 ns |  5.8594 |  0.9766 |       - |  100032 B |
| SystemTextJson_Document       | ParseToSingleValue | 129,150.4 ns |   231.77 ns |  1,207.91 ns | 129,038.2 ns |       - |       - |       - |     128 B |
| LightJson                     | ParseToSingleValue | 350,286.6 ns | 1,738.87 ns |  9,031.67 ns | 351,680.0 ns | 32.2266 | 14.1602 |       - |  542440 B |
| NewtonsoftJson                | ParseToSingleValue | 471,769.1 ns | 2,251.02 ns | 11,731.73 ns | 473,176.2 ns | 36.6211 | 26.3672 |       - |  618136 B |

*JsonSpan doesnt parse on data insersion. Hence the results of JustParse.
**JsonSpan as it cant store the nodes, parses and reads the nodes as JsonSpan then pins them to memory as JsonMemory before running ToString

Test format:  
JustParse			= Create object of library and parse json file into it. No output requested from object.  
ParseToString		= Create object of library and parse json file into it. Take created object and request string of object using all ToString parameters.  
ParseReformat		= Create object of library and parse json file into it. Created object is iterated over and some values are used to create a new object then request a string of object using all parameters.  
ParseToSingleValue	= Create object of library and parse json file into it. Receives index and string path and aquires a single value using all ToString parameters.  

## JsonMemory
Minimal allocation without compromise on time.  
Access constructors via static JsonMemory.ParseJson ext.  
Object and Array types both accessible via indexer, dictionary is not implemented.  
Object and Array types both accessible via foreach.
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
functions the same as JsonMemory but without the overhead of storing the data in arrays.  

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
  
JsonMemory utilises the Enumerator provided in its contained array, JsonSpan is merged with the Enumerator pattern to provide this functionality.  

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

JsonSpan as it has no backing container needs to read to reaquire values while it Enumerates, it tracks its current position and reads forward to the index requested. If the index is less than the current position, it will reset and read to the index requested.  

```CS
private void Index(string jsonData) {
    JsonSpan JSpan = new JsonSpan(jsonData);
    if (JSpan.TryGetIndex(2, out JsonSpan JSpan2)) {
        ...
    }
    // From here, JSpan is at index 2, going backwards calls Reset() internally, going forward continues where it left off from index 2
}
```  

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

### Type checking

JsonType is a flagged enum so equivalence wont work for containers unless its the exact value. This is because Disposable is contained within the Type value.  
To make the type checking quick and accurate, Is* properties are made available for type checking performing the HasFlag check for you.  

```CS
private void CheckType(JsonMemory json) {
	// Wrong
    if (json.Type == JsonType.Object) {
		// Wont get here unless backing array was made with non disposable properties
	}
	
	// Correct
    if (json.IsObject) {
		// Property performs flag check directly so if its a object it will always enter
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

## Advanced  

### ArrayPool

To minimise new array creation, new arrays are created and recycled inside JsonArrayPool found inside NanoJsonStatics.JsonContainerPool, some settings are available to manipulate its static sizes.

```CS
private void UpdateJsonPools(int smallestArray, int largestArray, int numberOfArrays) {
    NanoJsonStatics.JsonContainerPool.PoolMinArrayLength = smallestArray;
    NanoJsonStatics.JsonContainerPool.PoolMaxArrayLength = largestArray;
    NanoJsonStatics.JsonContainerPool.ArraysPerBucket = numberOfArrays;
}
```   

The container itself is stored Lazy so it will only be created when first used, and will be disposed of when the application ends.  
To quickly access the pool, use the static NanoJsonStatics.JsonContainerPool to access the pool.

```CS
private void RentAPool(int size) {
    JsonMemory[] rent = NanoJsonStatics.JsonContainerPool.Rent(size);

	.. // Do stuff with the rented array

    NanoJsonStatics.JsonContainerPool.Rent(rent);
}
```   

JsonArrayPool is utilised in JsonMemory initially as the temporary store for parsed values, these values are then copied into another rented container which then is the value for ContainedValues.  
JsonSpan does not use JsonArrayPool, however it does use ArrayPool`<char>`.Shared for string constructon as while slow enough in comparison due to reading, to get the correct value for string.Create, it would need a second read effectively doubling processing times.  
JsonMemory does not have this flaw and is able to quickly determin the required size and build the string via string.Create, eliminating the need for a char buffer.  

### JsonReader

JsonReader is a ref struct that utilises System.Numerics Vector to read the json data in 16 byte chunks compared to per character array searching.  
While covering a short distance would be slower than a simple char array search, the speed of reading beyond the first chunk is significantly improved.  
Despite the name it doesnt read per Json value/object ext, its purpose is to traverse char data efficiently.

```CS
private void ReadJson(ReadOnlySpan<char> data) {
    JsonReader reader = new JsonReader(data);

	// Advance forward
	reader.Advance(4);
	reader.AdvanceTo('}');
	reader.AdvanceToNot(' ');
	reader.AdvanceToNotWhiteSpace();

	// Retreat back
	reader.Retreat(4);
	reader.RetreatTo('}');
	reader.RetreatToNot(' ');
	reader.RetreatToNotWhiteSpace();
}
```   
