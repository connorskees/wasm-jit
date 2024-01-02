// clang++ files/array-contains.c -ObjC++ --compile --target=wasm32-unknown-unknown-wasm --optimize=3 --output foo.wasm

#define WASM_EXPORT_AS(name) __attribute__((export_name(name)))
#define WASM_EXPORT(symbol) WASM_EXPORT_AS(#symbol) symbol

bool WASM_EXPORT(contains) (int* nums, int num_elems, int target) {
    for (int i = 0; i < num_elems; i++) {
        if (nums[i] == target) {
            return true;
        }
    }
    return false;
}

