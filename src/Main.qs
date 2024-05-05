namespace MyQuantumProgram {
    open Microsoft.Quantum.Random;
    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Math;

    function BoolArrayAsStringHelper(arr : Bool[], index : Int, str : String) : String {
        if (index == Length(arr)) {
            return str;
        }

        // Create a new string with the concatenated value
        let newStr = str + $"{arr[index]}, ";
        // Recur for the next element
        return BoolArrayAsStringHelper(arr, index + 1, newStr);
    }

    // Turn seperate Bool values into one long string
    function BoolArrayAsString(arr : Bool[]) : String {
        return "[" + BoolArrayAsStringHelper(arr, 0, "") + "]";
    }

    operation GenerateRandomBit() : Result {
        // Allocate a qubit.
        use q = Qubit();

        // Set the qubit into superposition of 0 and 1 using the Hadamard gate
        H(q);

        // At this point the qubit `q` has 50% chance of being measured in the
        // |0〉 state and 50% chance of being measured in the |1〉 state.
        // Measure the qubit value using the `M` operation, and store the
        // measurement value in the `result` variable.
        let result = M(q);

        // Reset qubit to the |0〉 state.
        // Qubits must be in the |0〉 state by the time they are released.
        Reset(q);

        // Return the result of the measurement.
        return result;
    }

    operation RandomArray(N : Int, name : String) : Bool[]  {
        // Initialize an empty array to store the random integer values
        mutable intArray = [];
        
        // Generate random bits values (0 or 1) and store them in the array
        for i in 0 .. N - 1 {
            // Create an array wit
            set intArray += [GenerateRandomBit()];
        }
  
        // Initialize an empty array to store the boolean values
        mutable boolArray = [];
  
        // Convert the integer array into a boolean array
        for i in 0 .. N - 1 {
            set boolArray += [intArray[i] == One];
        }

        // Log out the entire boolArray before returning it

        Message("\n" + $"Contents of {name}: " + "\n" + BoolArrayAsString(boolArray));
  
        // Return the array containing boolean values
        return boolArray;
    }

    operation PrepareAlicesQubits (qs : Qubit[], bases : Bool[], bits : Bool[]) : Unit {
        // Iterate over all the qubits to prepare each one
        for i in 0 .. Length(qs) - 1 {
            if bits[i] {
                X(qs[i]);
            }
            if bases[i] {
                H(qs[i]);
            }
        }
    }

    function Mapped<'T, 'U> (mapper : ('T -> 'U), array : 'T[]) : 'U[] {
        let length = Length(array);
        
        if length == 0 {
            return [];
        }

        let first = mapper(array[0]);
        mutable retval = [first, size = length];
        for idx in 1..length - 1 {
            set retval w/= idx <- mapper(array[idx]);
        }
        return retval;
    }

    // Turn a Result type value into a Bool type value
    function ResultAsBool(input : Result) : Bool {
        return input == Zero ? false | true;
    }

    // Turn a Result type array into a Bool type array
    function ResultArrayAsBoolArray(input : Result[]) : Bool[] {
        return Mapped(ResultAsBool, input);
    }

    // Generates a Result[] with the measure values
    operation MeasureEachZ (targets : Qubit[]) : Result[] {
        let len = Length(targets);
        mutable results = [Zero, size=len];

        for i in 0..(len - 1) {
            set results w/= i <- M(targets[i]);
        }
  
        return results;
    }

    // Puts the qubits into superposition to be sent for measurement
    operation MeasureBobsQubits (qs : Qubit[], bases : Bool[]) : Bool[] {
        // Iterate over all the qubits
        for i in 0 .. Length(qs) - 1 {
            if bases[i] {
                H(qs[i]);
            }
        }
      
        // MeasureEachZ(qs) produces Result[] which is taken by 
        // ResultArrayAsBoolArray as the input.
        return ResultArrayAsBoolArray(MeasureEachZ(qs));
    }

    // Used to trim the array size when creating the tuple array
    function Min (values : Int[]) : Int {
        mutable min = values[0];
        let nTerms = Length(values);

        for idx in 0 .. nTerms - 1 {
            if values[idx] < min {
            set min = values[idx];
            }
        }
        return min;
    }

    // Turns three seperate arrays into an array of tuples
    // [a] [b] [c] ==> [[a,b,c]]
    function Zipped3<'T1, 'T2, 'T3> (first : 'T1[], second : 'T2[], third : 'T3[]) : ('T1, 'T2, 'T3)[] {
        let nElements = Min([Length(first), Length(second), Length(third)]);

        if nElements == 0 {
            return [];
        }

        mutable output = [(first[0], second[0], third[0]), size = nElements];

        for idxElement in 1 .. nElements - 1 {
            set output w/= idxElement <- (first[idxElement], second[idxElement], third[idxElement]);
        }
        
        return output;
    }

    function GenerateSharedKey (basesAlice : Bool[], basesBob : Bool[], measurementsBob : Bool[]) : Bool[] {
        // Declare empty array key for storing the required value of key
        mutable key = [];
  
        // Iteration over all the qubit sending attempts
        // Zipped3 function ensures we iterate over a tuple of 3 items.
        for (a, b, bit) in Zipped3(basesAlice, basesBob, measurementsBob) {
            if a == b {
                set key += [bit]; // Add bit to the key in case bases of both Alice and Bob matches
            }
        }
  
        // Step 3: Return the required key
        return key;
    }

    function CheckKeysMatch (keyAlice : Bool[], keyBob : Bool[], errorRate : Int) : Bool {
        let N = Length(keyAlice);
  
        // Declare a variable to count the number of mismatched bits
        mutable mismatchCount = 0;
  
        for i in 0 .. N - 1 {
            if keyAlice[i] != keyBob[i] {
                set mismatchCount += 1; // Increment the counter whenever a mismatch is found
            }
        }

        // return true if probability of mismatched bits is less than the Error Rate provided
        return IntAsDouble(mismatchCount) / IntAsDouble(N) <= IntAsDouble(errorRate) / 100.0;
    }

    operation Eavesdrop (q : Qubit, basis : Bool) : Bool {
        // Measurement along X axis if basis is diagonal basis and Z axis, otherwise.
        return ResultAsBool(Measure([basis ? PauliX | PauliZ], [q]));
    }

    @EntryPoint()
    operation Run_BB84ProtocolWithEavesdropper() : Unit {
        // Require a nearly perfect match of the shared keys for success
        // (1% of the characters are allowed to be different).
        let threshold = 1;

        use qs = Qubit[20];
        // 1. Choose random basis and bits to encode
        let basesAlice = RandomArray(Length(qs), "Alice Bases");
        let bitsAlice = RandomArray(Length(qs), "Alice Bits");

        // 2. Alice prepares her qubits
        PrepareAlicesQubits(qs, basesAlice, bitsAlice);
        
        // 3. Eve eavesdrops on all qubits, guessing the basis at random prepares her qubits
        for q in qs {
            let n = Eavesdrop(q, (GenerateRandomBit() == Zero ? false|true));
        }

        // 4. Bob chooses random basis to measure in
        let basesBob = RandomArray(Length(qs), "Bob Bases");

        // 5. Bob measures Alice's qubits
        let bitsBob = MeasureBobsQubits(qs, basesBob);

        // 6. Generate shared key
        let keyAlice = GenerateSharedKey(basesAlice, basesBob, bitsAlice);
        let keyBob = GenerateSharedKey(basesAlice, basesBob, bitsBob);

        // 7. Ensure at least the minimum percentage of bits match
        if CheckKeysMatch(keyAlice, keyBob, threshold) {
            Message("\n" + $"Successfully Generated Keys");
            Message($"Alice: {keyAlice}");
            Message($"Bob: {keyBob}" + "\n");
        } else {
            Message("\n" + $"Caught an eavesdropper! Discarding the keys...");
            Message($"Alice: {keyAlice}");
            Message($"Bob: {keyBob}" + "\n");
        }
    }
}
