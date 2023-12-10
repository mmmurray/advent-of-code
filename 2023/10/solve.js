#!/usr/bin/env node
process.env['COMPILER_VERSION'] = process.env['COMPILER_VERSION'] || '0.0.131'
process.env['COMPILER_CORE_PATH'] = process.env['COMPILER_CORE_PATH'] || __dirname + '/core'

const _use64Bit = process.env['COMPILER_ARCHITECTURE'] === '64'
const _memSize = 1024 * 1024 * 200
const _memData = new ArrayBuffer(_memSize)
const _memView = new DataView(_memData)
let _memBase = 0

function _pointerToString(string) {
  const array = m_stringToArray(string)
  const size = m_arraySize(array)
  let jsString = ''
  for (let i = 0; i < size; i++) {
    const codePoint = m_arrayGet(array, i)
    jsString += String.fromCodePoint(codePoint)
  }
  return jsString
}

function _stringToPointer(jsString) {
  const size = jsString.length
  const array = m_arrayCreate(size)
  for (let i = 0; i < jsString.length; i++) {
    m_arraySet(array, i, jsString.codePointAt(i))
  }
  return m_stringCreate(array)
}

function _listToArray(list) {
  const array = []
  const size = m_listSize(list)
  for (let i = 0; i < size; i++) {
    array.push(m_listGet(list, i))
  }
  return array
}

function $alloc(size) {
  if (_memBase + size >= _memSize) {
    throw new Error('Out of cheese')
  }

  const ptr = _memBase
  _memBase += size
  return ptr
}

function $load(address) {
  return _use64Bit ? Number(_memView.getBigInt64(address)) : _memView.getInt32(address)
}

function $store(address, value) {
  if (_use64Bit) {
    _memView.setBigInt64(address, BigInt(value))
  } else {
    _memView.setInt32(address, value)
  }
}

function $exit(code) {
  process.exit(code)
}

function $panic(message) {
  const error = new Error(_pointerToString(message))
  error.name = ''
  console.error(error)
  $exit(1)
}

function $assertEqual(a, b) {
  if (a !== b) {
    $panic(_stringToPointer('Assertion failed'))
  }
}

function $print(pointer) {
  console.log(_pointerToString(pointer))
}

function $pathNormalize(path) {
  const { normalize } = require('path')
  return _stringToPointer(normalize(_pointerToString(path)))
}

function $pathRelative(from, to) {
  const { relative } = require('path')
  return _stringToPointer(relative(_pointerToString(from), _pointerToString(to)))
}

function $pathCurrent() {
  return _stringToPointer(process.cwd())
}

function $fileExists(path) {
  const { existsSync } = require('fs')
  const exists = existsSync(_pointerToString(path))
  return exists ? 1 : 0
}

function $fileRead(path) {
  const { normalize } = require('path')
  const { readFileSync } = require('fs')
  const file = readFileSync(normalize(_pointerToString(path)), 'utf-8')
  return _stringToPointer(file)
}

function $fileWrite(path, data) {
  const { writeFileSync } = require('fs')
  writeFileSync(_pointerToString(path), _pointerToString(data), 'utf-8')
}

function $fileList(path) {
  const { readdirSync } = require('fs')
  const items = readdirSync(_pointerToString(path))
  const size = items.length
  const array = m_arrayCreate(size)
  for (let i = 0; i < size; i++) {
    m_arraySet(array, i, _stringToPointer(items[i]))
  }
  return array
}

function $compilerGetIntByteSize() {
  return _use64Bit ? 8 : 4
}

function $compilerGetVersion() {
  return _stringToPointer(process.env['COMPILER_VERSION'])
}

function $compilerGetCorePath() {
  return _stringToPointer(process.env['COMPILER_CORE_PATH'])
}

function $time(marker) {
  console.time(_pointerToString(marker))
}

function $timeEnd(marker) {
  console.timeEnd(_pointerToString(marker))
}

function $wasmExecI32(bytesList, input) {
  const bytesSize = m_listSize(bytesList)
  const wasmCode = new Uint8Array(bytesSize)
  for (let i = 0; i < bytesSize; i++) {
    wasmCode[i] = m_listGet(bytesList, i)
  }

  const memory = new WebAssembly.Memory({ initial: 256 });

  const loadString = (ptr) => {
    const header = new Uint32Array(memory.buffer, ptr, 1);
    const data = new Uint32Array(memory.buffer, ptr + 4, header[0]);
    return String.fromCodePoint(...data);
  };

  const wasmModule = new WebAssembly.Module(wasmCode);
  const wasmInstance = new WebAssembly.Instance(wasmModule, {
    core: {
      memory,
      square: (x) => x * x,
      print: (ptr) => {
        console.log(loadString(ptr))
        return 0
      }
    }
  });
  const main = wasmInstance.exports.main;
  if (typeof main !== 'function') {
    throw new Error('No exported main function')
  }

  const result = main(input)
  return result
}

function $pathCurrent() {
  return _stringToPointer(process.cwd())
}

function $pathTemp() {
  const { tmpdir } = require('os')

  return _stringToPointer(tmpdir())
}

function $pathJoin(a, b) {
  const { join } = require('path')

  return _stringToPointer(join(_pointerToString(a), _pointerToString(b)))
}

function $processEnv(name) {
  return _stringToPointer(process.env[_pointerToString(name)] ?? '')
}

function $processRun(command, argsList, cwd, printOutput) {
  const { spawnSync } = require('child_process');
  const args = _listToArray(argsList).map(_pointerToString)

  const result = spawnSync(_pointerToString(command), args, {
    cwd: _pointerToString(cwd),
    env: process.env,
    stdio: printOutput ? 'inherit' : 'pipe',
  });

  const output = m_arrayCreate(3)
  m_arraySet(output, 0, _stringToPointer(String(result.status)))
  m_arraySet(output, 1, _stringToPointer(result.stdout ? result.stdout.toString('utf-8') : ''))
  m_arraySet(output, 2, _stringToPointer(result.stderr ? result.stderr.toString('utf-8') : ''))
  return output
}


;[1,48,16,65,115,115,101,114,116,105,111,110,32,102,97,105,108,101,100,1,39,1,39,1,39,1,39,9,101,120,112,101,99,116,101,100,32,14,32,98,117,116,32,114,101,99,101,105,118,101,100,32,18,65,115,115,101,114,116,105,111,110,32,102,97,105,108,101,100,58,32,1,40,2,44,32,1,41,1,40,2,44,32,1,41,1,10,2,32,78,2,32,69,2,32,83,2,32,87,1,35,1,126,1,46,1,10,9,105,110,112,117,116,46,116,120,116,7,80,97,114,116,32,49,58,4,55,49,48,55,7,80,97,114,116,32,50,58,3,50,56,49,].forEach((x, i) => { $store(i * 4, x) })
_memBase = 568

function c_Some() {
  const ptr = $alloc(8)
  $store(ptr, 0)
  return ptr
}
function g_Some_value(ptr) {
  return $load(ptr + 4)
}
function s_Some_value(ptr, value) {
  return $store(ptr + 4, value)
}
function c_None() {
  const ptr = $alloc(4)
  $store(ptr, 1)
  return ptr
}

function m_optionValue(m_default, m_option) {
  let m_result = (() => {
    let ptr = m_option
    const tag = $load(ptr)
    if (tag === 0) { // Some
      const m_value = g_Some_value(ptr)
      return m_value
    }
    if (tag === 1) { // None
      return m_default
    }
  })()
  return m_result
}
  let m_compilerIntByteSize = $compilerGetIntByteSize()
function c_Array() {
  const ptr = $alloc(4)
  return ptr
}
function g_Array_items(ptr) {
  return $load(ptr + 0)
}
function s_Array_items(ptr, value) {
  return $store(ptr + 0, value)
}

function m_arrayFrom(m_items) {
  return (() => {
    let ptr = c_Array()
    s_Array_items(ptr, m_items)
    return ptr
  })()
}
function m_arrayCreate(m_size) {
  let m_bytes = Math.trunc(m_compilerIntByteSize + Math.trunc(m_size * m_compilerIntByteSize))
  let m_items = $alloc(m_bytes)
  $store(m_items, m_size)
  return m_arrayFrom(m_items)
}
function m_arraySize(m_array) {
  let m_items = g_Array_items(m_array)
  return $load(m_items)
}
function m_arrayIndexToByteOffset(m_items, m_index) {
  return Math.trunc(m_items + Math.trunc(m_compilerIntByteSize * Math.trunc(m_index + 1)))
}
function m_arraySet(m_array, m_index, m_value) {
  let m_items = g_Array_items(m_array)
  $store(m_arrayIndexToByteOffset(m_items, m_index), m_value)
}
function m_arrayGet(m_array, m_index) {
  let m_items = g_Array_items(m_array)
  return $load(m_arrayIndexToByteOffset(m_items, m_index))
}
function m_arrayRange(m_start, m_end) {
  let m_size = Math.trunc(m_end - m_start)
  let m_array = m_arrayCreate(m_size)
  let m_i = 0
while (Math.trunc(m_i < m_size)) {
  m_arraySet(m_array, m_i, Math.trunc(m_i + m_start))
  m_i = Math.trunc(m_i + 1)
}
  return m_array
}
function m_arraySum(m_array) {
  let m_size = m_arraySize(m_array)
  let m_sum = 0
  let m_i = 0
while (Math.trunc(m_i < m_size)) {
  let m_value = m_arrayGet(m_array, m_i)
  m_sum = Math.trunc(m_sum + m_value)
  m_i = Math.trunc(m_i + 1)
}
  return m_sum
}
function m_arrayCopy(m_size, m_target, m_targetOffset, m_source, m_sourceOffset) {
  let m_targetSize = m_arraySize(m_target)
  let m_sourceSize = m_arraySize(m_source)
  let m_i = 0
while (Math.trunc(m_i < m_size)) {
  m_arraySet(m_target, Math.trunc(m_i + m_targetOffset), m_arrayGet(m_source, Math.trunc(m_i + m_sourceOffset)))
  m_i = Math.trunc(m_i + 1)
}
}
function m_arrayEquals(m_a, m_b) {
  let m_aSize = m_arraySize(m_a)
  let m_bSize = m_arraySize(m_b)
if (Math.trunc(m_aSize !== m_bSize)) {
  return 0
}
  let m_i = 0
while (Math.trunc(m_i < m_aSize)) {
if (Math.trunc(m_arrayGet(m_a, m_i) !== m_arrayGet(m_b, m_i))) {
  return 0
}
  m_i = Math.trunc(m_i + 1)
}
  return 1
}
function m_arrayConcat(m_a, m_b) {
  let m_size = Math.trunc(m_arraySize(m_a) + m_arraySize(m_b))
  let m_c = m_arrayCreate(m_size)
  m_arrayCopy(m_arraySize(m_a), m_c, 0, m_a, 0)
  m_arrayCopy(m_arraySize(m_b), m_c, m_arraySize(m_a), m_b, 0)
  return m_c
}
function m_arrayContainsAt(m_haystack, m_needle, m_offset) {
  let m_haystackSize = m_arraySize(m_haystack)
  let m_needleSize = m_arraySize(m_needle)
if (Math.trunc(Math.trunc(m_needleSize + m_offset) > m_haystackSize)) {
  return 0
}
  let m_i = 0
while (Math.trunc(m_i < m_needleSize)) {
if (Math.trunc(m_arrayGet(m_haystack, Math.trunc(m_i + m_offset)) !== m_arrayGet(m_needle, m_i))) {
  return 0
}
  m_i = Math.trunc(m_i + 1)
}
  return 1
}
function m_arrayStartsWith(m_haystack, m_needle) {
  return m_arrayContainsAt(m_haystack, m_needle, 0)
}
function m_arraySlice(m_array, m_start, m_end) {
  let m_size = Math.trunc(m_end - m_start)
  let m_subArray = m_arrayCreate(m_size)
  m_arrayCopy(m_size, m_subArray, 0, m_array, m_start)
  return m_subArray
}
function m_arrayClone(m_array) {
  let m_size = m_arraySize(m_array)
  let m_newArray = m_arrayCreate(m_size)
  m_arrayCopy(m_size, m_newArray, 0, m_array, 0)
  return m_newArray
}
function c_List() {
  const ptr = $alloc(12)
  return ptr
}
function g_List_capacity(ptr) {
  return $load(ptr + 0)
}
function s_List_capacity(ptr, value) {
  return $store(ptr + 0, value)
}
function g_List_items(ptr) {
  return $load(ptr + 4)
}
function s_List_items(ptr, value) {
  return $store(ptr + 4, value)
}
function g_List_size(ptr) {
  return $load(ptr + 8)
}
function s_List_size(ptr, value) {
  return $store(ptr + 8, value)
}

  let m_LIST_DEFAULT_CAPACITY = 32
function m_listCreate(m_capacity) {
  return (() => {
    let ptr = c_List()
    s_List_capacity(ptr, m_capacity)
    s_List_items(ptr, m_arrayCreate(m_capacity))
    s_List_size(ptr, 0)
    return ptr
  })()
}
function m_listCreateDefault() {
  return m_listCreate(m_LIST_DEFAULT_CAPACITY)
}
function m_listSize(m_list) {
  return g_List_size(m_list)
}
function m_listSet(m_list, m_index, m_value) {
  m_arraySet(g_List_items(m_list), m_index, m_value)
}
function m_listGet(m_list, m_index) {
  return m_arrayGet(g_List_items(m_list), m_index)
}
function m_listEquals(m_a, m_b) {
  let m_aSize = m_listSize(m_a)
  let m_bSize = m_listSize(m_b)
if (Math.trunc(m_aSize !== m_bSize)) {
  return 0
}
  let m_i = 0
while (Math.trunc(m_i < m_aSize)) {
if (Math.trunc(m_listGet(m_a, m_i) !== m_listGet(m_b, m_i))) {
  return 0
}
  m_i = Math.trunc(m_i + 1)
}
  return 1
}
function m_listPush(m_list, m_value) {
  let m_capacity = g_List_capacity(m_list)
  let m_size = g_List_size(m_list)
  let m_items = g_List_items(m_list)
if (Math.trunc(m_size === m_capacity)) {
  let m_newCapacity = Math.trunc(m_capacity * 2)
  let m_newItems = m_arrayCreate(m_newCapacity)
  m_arrayCopy(m_capacity, m_newItems, 0, m_items, 0)
  s_List_capacity(m_list, m_newCapacity)
  s_List_items(m_list, m_newItems)
}
  m_listSet(m_list, m_size, m_value)
  s_List_size(m_list, Math.trunc(m_size + 1))
}
function m_listPushList(m_a, m_b) {
  let m_itemIndex = 0
while (Math.trunc(m_itemIndex < m_listSize(m_b))) {
  let m_item = m_listGet(m_b, m_itemIndex)
  m_listPush(m_a, m_item)
  m_itemIndex = Math.trunc(m_itemIndex + 1)
}
}
function m_listConcat(m_a, m_b) {
  let m_newList = m_listClone(m_a)
  m_listPushList(m_newList, m_b)
  return m_newList
}
function m_listFromArray(m_array) {
  let m_list = m_listCreateDefault()
  let m_size = m_arraySize(m_array)
  let m_i = 0
while (Math.trunc(m_i < m_size)) {
  m_listPush(m_list, m_arrayGet(m_array, m_i))
  m_i = Math.trunc(m_i + 1)
}
  return m_list
}
function m_listToArray(m_list) {
  let m_size = g_List_size(m_list)
  let m_items = g_List_items(m_list)
  let m_array = m_arrayCreate(m_size)
  m_arrayCopy(m_size, m_array, 0, m_items, 0)
  return m_array
}
function m_listClone(m_list) {
  return (() => {
    let ptr = c_List()
    s_List_capacity(ptr, g_List_capacity(m_list))
    s_List_size(ptr, g_List_size(m_list))
    s_List_items(ptr, m_arrayClone(g_List_items(m_list)))
    return ptr
  })()
}
function m_listSlice(m_list, m_start, m_end) {
  return m_listFromArray(m_arraySlice(m_listToArray(m_list), m_start, m_end))
}



function m_charIsWhitespace(m_c) {
  return Math.trunc(Math.trunc(m_c === 10) || Math.trunc(Math.trunc(m_c === 9) || Math.trunc(m_c === 32)))
}
function m_charIsNumeric(m_c) {
  return Math.trunc(Math.trunc(m_c >= 48) && Math.trunc(m_c <= 57))
}
function m_charIsHexadecimal(m_c) {
  return Math.trunc(Math.trunc(Math.trunc(m_c >= 48) && Math.trunc(m_c <= 57)) || Math.trunc(Math.trunc(m_c >= 65) && Math.trunc(m_c <= 70)))
}
function m_charIsLower(m_c) {
  return Math.trunc(Math.trunc(m_c >= 97) && Math.trunc(m_c <= 122))
}
function m_charIsUpper(m_c) {
  return Math.trunc(Math.trunc(m_c >= 65) && Math.trunc(m_c <= 90))
}
function m_charIsAlpha(m_c) {
  return Math.trunc(m_charIsLower(m_c) || m_charIsUpper(m_c))
}
function m_charIsAlphaNumeric(m_c) {
  return Math.trunc(m_charIsAlpha(m_c) || m_charIsNumeric(m_c))
}
function m_charToLower(m_c) {
if (m_charIsUpper(m_c)) {
  return Math.trunc(m_c + 32)
}
  return m_c
}
function m_charToUpper(m_c) {
if (m_charIsLower(m_c)) {
  return Math.trunc(m_c - 32)
}
  return m_c
}
function m_charToString(m_c) {
  let m_chars = m_arrayCreate(1)
  m_arraySet(m_chars, 0, m_c)
  return m_stringCreate(m_chars)
}



function c_String() {
  const ptr = $alloc(4)
  return ptr
}
function g_String_chars(ptr) {
  return $load(ptr + 0)
}
function s_String_chars(ptr, value) {
  return $store(ptr + 0, value)
}

function m_stringCreate(m_chars) {
  return (() => {
    let ptr = c_String()
    s_String_chars(ptr, m_chars)
    return ptr
  })()
}
function m_stringClone(m_string) {
  return m_stringCreate(m_arrayClone(g_String_chars(m_string)))
}
function m_stringSize(m_string) {
  return m_arraySize(g_String_chars(m_string))
}
function m_stringGet(m_string, m_index) {
  return m_arrayGet(g_String_chars(m_string), m_index)
}
function m_stringSet(m_string, m_index, m_char) {
  m_arraySet(g_String_chars(m_string), m_index, m_char)
}
function m_stringEquals(m_a, m_b) {
  return m_arrayEquals(g_String_chars(m_a), g_String_chars(m_b))
}
function m_stringStartsWith(m_haystack, m_needle) {
  return m_arrayStartsWith(g_String_chars(m_haystack), g_String_chars(m_needle))
}
function m_stringContainsAt(m_haystack, m_needle, m_offset) {
  return m_arrayContainsAt(g_String_chars(m_haystack), g_String_chars(m_needle), m_offset)
}
function m_stringContains(m_haystack, m_needle) {
  let m_offset = 0
  let m_maxOffset = Math.trunc(m_stringSize(m_haystack) - m_stringSize(m_needle))
while (Math.trunc(m_offset <= m_maxOffset)) {
if (m_stringContainsAt(m_haystack, m_needle, m_offset)) {
  return 1
}
  m_offset = Math.trunc(m_offset + 1)
}
  return 0
}
function m_stringConcat(m_a, m_b) {
  return m_stringCreate(m_arrayConcat(g_String_chars(m_a), g_String_chars(m_b)))
}
function m_stringSlice(m_string, m_start, m_end) {
  return m_stringCreate(m_arraySlice(g_String_chars(m_string), m_start, m_end))
}
function m_stringToArray(m_string) {
  return g_String_chars(m_string)
}
function m_stringToLower(m_string) {
  let m_newString = m_stringClone(m_string)
  let m_size = m_stringSize(m_newString)
  let m_i = 0
while (Math.trunc(m_i < m_size)) {
  let m_value = m_stringGet(m_newString, m_i)
  let m_newValue = m_charToLower(m_value)
  m_stringSet(m_newString, m_i, m_newValue)
  m_i = Math.trunc(m_i + 1)
}
  return m_newString
}
function m_stringToUpper(m_string) {
  let m_newString = m_stringClone(m_string)
  let m_size = m_stringSize(m_newString)
  let m_i = 0
while (Math.trunc(m_i < m_size)) {
  let m_value = m_stringGet(m_newString, m_i)
  let m_newValue = m_charToUpper(m_value)
  m_stringSet(m_newString, m_i, m_newValue)
  m_i = Math.trunc(m_i + 1)
}
  return m_newString
}
function m_stringFromNumber(m_n) {
if (Math.trunc(m_n === 0)) {
  return m_stringCreate(m_arrayFrom(0))
}
  let m_negative = Math.trunc(m_n < 0)
  let m_offset = 0
if (m_negative) {
  m_n = Math.trunc(0 - m_n)
  m_offset = 1
}
  let m_buffer = m_arrayCreate(20)
  let m_size = 0
while (Math.trunc(m_n > 0)) {
  let m_d = Math.trunc(Math.trunc(m_n % 10) + 48)
  m_arraySet(m_buffer, m_size, m_d)
  m_size = Math.trunc(m_size + 1)
  m_n = Math.trunc(m_n / 10)
}
  let m_i = 0
  let m_chars = m_arrayCreate(Math.trunc(m_size + m_offset))
if (m_negative) {
  m_arraySet(m_chars, 0, 45)
}
while (Math.trunc(m_i < m_size)) {
  let m_c = m_arrayGet(m_buffer, Math.trunc(m_size - Math.trunc(m_i + 1)))
  m_arraySet(m_chars, Math.trunc(m_i + m_offset), m_c)
  m_i = Math.trunc(m_i + 1)
}
  return m_stringCreate(m_chars)
}
function m_stringToNumber(m_s) {
  let m_n = 0
  let m_size = m_stringSize(m_s)
  let m_charIndex = 0
  let m_negative = Math.trunc(Math.trunc(m_size > 0) && Math.trunc(m_stringGet(m_s, 0) === 45))
if (m_negative) {
  m_charIndex = Math.trunc(m_charIndex + 1)
}
  let m_hex = Math.trunc(Math.trunc(m_size > 2) && Math.trunc(Math.trunc(m_stringGet(m_s, 0) === 48) && Math.trunc(m_stringGet(m_s, 1) === 120)))
if (m_hex) {
  m_charIndex = Math.trunc(m_charIndex + 2)
while (Math.trunc(m_charIndex < m_size)) {
  let m_char = m_stringGet(m_s, m_charIndex)
  let m_digitValue = 0
if (Math.trunc(Math.trunc(m_char >= 65) && Math.trunc(m_char <= 70))) {
  m_digitValue = Math.trunc(Math.trunc(m_char - 65) + 10)
} else {
if (Math.trunc(Math.trunc(m_char >= 48) && Math.trunc(m_char <= 57))) {
  m_digitValue = Math.trunc(m_char - 48)
} else {
break
}
}
  m_n = Math.trunc(Math.trunc(m_n * 16) + m_digitValue)
  m_charIndex = Math.trunc(m_charIndex + 1)
}
} else {
while (Math.trunc(m_charIndex < m_size)) {
  let m_char = m_stringGet(m_s, m_charIndex)
if (Math.trunc(Math.trunc(m_char < 48) || Math.trunc(m_char > 57))) {
break
}
  let m_digitValue = Math.trunc(m_char - 48)
  m_n = Math.trunc(Math.trunc(m_n * 10) + m_digitValue)
  m_charIndex = Math.trunc(m_charIndex + 1)
}
}
if (m_negative) {
  return Math.trunc(0 - m_n)
}
  return m_n
}
function m_stringTrimStart(m_s) {
  let m_size = m_stringSize(m_s)
  let m_offset = 0
while (Math.trunc(m_offset < m_size)) {
  let m_char = m_stringGet(m_s, m_offset)
if (Math.trunc(!m_charIsWhitespace(m_char))) {
break
}
  m_offset = Math.trunc(m_offset + 1)
}
  return m_stringSlice(m_s, m_offset, m_size)
}
function m_stringTrimEnd(m_s) {
  let m_size = m_stringSize(m_s)
  let m_offset = Math.trunc(m_size - 1)
while (Math.trunc(m_offset >= 0)) {
  let m_char = m_stringGet(m_s, m_offset)
if (Math.trunc(!m_charIsWhitespace(m_char))) {
break
}
  m_offset = Math.trunc(m_offset - 1)
}
  return m_stringSlice(m_s, 0, Math.trunc(m_offset + 1))
}
function m_stringTrim(m_s) {
  return m_stringTrimStart(m_stringTrimEnd(m_s))
}
function m_stringSplitBy(m_input, m_delimiter) {
  let m_parts = m_listCreateDefault()
  let m_size = m_stringSize(m_input)
  let m_partStart = 0
  let m_partEnd = 0
while (Math.trunc(m_partEnd < m_size)) {
  let m_char = m_stringGet(m_input, m_partEnd)
if (Math.trunc(m_char === m_delimiter)) {
  let m_part = m_stringSlice(m_input, m_partStart, m_partEnd)
  m_listPush(m_parts, m_part)
  m_partStart = Math.trunc(m_partEnd + 1)
}
  m_partEnd = Math.trunc(m_partEnd + 1)
}
  let m_part = m_stringSlice(m_input, m_partStart, m_partEnd)
  m_listPush(m_parts, m_part)
  return m_parts
}
function m_stringToLines(m_input) {
  return m_stringSplitBy(m_input, 10)
}



function c_StringBuilder() {
  const ptr = $alloc(4)
  return ptr
}
function g_StringBuilder_chars(ptr) {
  return $load(ptr + 0)
}
function s_StringBuilder_chars(ptr, value) {
  return $store(ptr + 0, value)
}

function m_stringBuilderCreate(m_capacity) {
  return (() => {
    let ptr = c_StringBuilder()
    s_StringBuilder_chars(ptr, m_listCreate(m_capacity))
    return ptr
  })()
}
function m_stringBuilderSize(m_stringBuilder) {
  let m_chars = g_StringBuilder_chars(m_stringBuilder)
  return m_listSize(m_chars)
}
function m_stringBuilderAppend(m_stringBuilder, m_string) {
  let m_chars = g_StringBuilder_chars(m_stringBuilder)
  let m_index = 0
  let m_size = m_stringSize(m_string)
while (Math.trunc(m_index < m_size)) {
  let m_char = m_stringGet(m_string, m_index)
  m_listPush(m_chars, m_char)
  m_index = Math.trunc(m_index + 1)
}
}
function m_stringBuilderBuild(m_stringBuilder) {
  let m_chars = g_StringBuilder_chars(m_stringBuilder)
  return m_stringCreate(m_listToArray(m_chars))
}
function m_assert(m_predicate) {
if (Math.trunc(!m_predicate)) {
  $panic(m_stringCreate(m_arrayFrom(8)))
}
}
function m_assertEqualString(m_actual, m_expected) {
if (Math.trunc(!m_stringEquals(m_actual, m_expected))) {
  let m_actualQuoted = m_stringConcat(m_stringCreate(m_arrayFrom(76)), m_stringConcat(m_actual, m_stringCreate(m_arrayFrom(84))))
  let m_expectedQuoted = m_stringConcat(m_stringCreate(m_arrayFrom(92)), m_stringConcat(m_expected, m_stringCreate(m_arrayFrom(100))))
  let m_reason = m_stringConcat(m_stringCreate(m_arrayFrom(108)), m_stringConcat(m_expectedQuoted, m_stringConcat(m_stringCreate(m_arrayFrom(148)), m_actualQuoted)))
  $panic(m_stringConcat(m_stringCreate(m_arrayFrom(208)), m_reason))
}
}
function m_assertEqual(m_actual, m_expected) {
if (Math.trunc(m_actual !== m_expected)) {
  m_assertEqualString(m_stringFromNumber(m_actual), m_stringFromNumber(m_expected))
}
}
function m_print(m_output) {
  $print(m_output)
}
function m_write(m_output) {
  m_print(m_stringFromNumber(m_output))
}
function m_exit(m_code) {
  $exit(m_code)
}
function m_fileExists(m_path) {
  return $fileExists(m_path)
}
function m_fileRead(m_path) {
  return $fileRead(m_path)
}
function m_fileWrite(m_path, m_fileData) {
  $fileWrite(m_path, m_fileData)
}
function m_pathCurrent() {
  return $pathCurrent()
}
function m_pathTemp() {
  return $pathTemp()
}
function m_pathJoin(m_a, m_b) {
  return $pathJoin(m_a, m_b)
}
function m_processEnv(m_name) {
  return $processEnv(m_name)
}
function c_ProcessRunInput() {
  const ptr = $alloc(16)
  return ptr
}
function g_ProcessRunInput_command(ptr) {
  return $load(ptr + 0)
}
function s_ProcessRunInput_command(ptr, value) {
  return $store(ptr + 0, value)
}
function g_ProcessRunInput_args(ptr) {
  return $load(ptr + 4)
}
function s_ProcessRunInput_args(ptr, value) {
  return $store(ptr + 4, value)
}
function g_ProcessRunInput_cwd(ptr) {
  return $load(ptr + 8)
}
function s_ProcessRunInput_cwd(ptr, value) {
  return $store(ptr + 8, value)
}
function g_ProcessRunInput_printOutput(ptr) {
  return $load(ptr + 12)
}
function s_ProcessRunInput_printOutput(ptr, value) {
  return $store(ptr + 12, value)
}

function c_ProcessRunResult() {
  const ptr = $alloc(12)
  return ptr
}
function g_ProcessRunResult_status(ptr) {
  return $load(ptr + 0)
}
function s_ProcessRunResult_status(ptr, value) {
  return $store(ptr + 0, value)
}
function g_ProcessRunResult_stdout(ptr) {
  return $load(ptr + 4)
}
function s_ProcessRunResult_stdout(ptr, value) {
  return $store(ptr + 4, value)
}
function g_ProcessRunResult_stderr(ptr) {
  return $load(ptr + 8)
}
function s_ProcessRunResult_stderr(ptr, value) {
  return $store(ptr + 8, value)
}

function m_processRun(m_input) {
  let m_cwd = (() => {
    let ptr = g_ProcessRunInput_cwd(m_input)
    const tag = $load(ptr)
    if (tag === 0) { // Some
      const m_value = g_Some_value(ptr)
      return m_value
    }
    if (tag === 1) { // None
      return m_pathCurrent()
    }
  })()
  let m_output = $processRun(g_ProcessRunInput_command(m_input), g_ProcessRunInput_args(m_input), m_cwd, g_ProcessRunInput_printOutput(m_input))
  return (() => {
    let ptr = c_ProcessRunResult()
    s_ProcessRunResult_status(ptr, m_stringToNumber(m_arrayGet(m_output, 0)))
    s_ProcessRunResult_stdout(ptr, m_arrayGet(m_output, 1))
    s_ProcessRunResult_stderr(ptr, m_arrayGet(m_output, 2))
    return ptr
  })()
}
function c_MapItem() {
  const ptr = $alloc(8)
  return ptr
}
function g_MapItem_key(ptr) {
  return $load(ptr + 0)
}
function s_MapItem_key(ptr, value) {
  return $store(ptr + 0, value)
}
function g_MapItem_value(ptr) {
  return $load(ptr + 4)
}
function s_MapItem_value(ptr, value) {
  return $store(ptr + 4, value)
}

function c_Map() {
  const ptr = $alloc(4)
  return ptr
}
function g_Map_items(ptr) {
  return $load(ptr + 0)
}
function s_Map_items(ptr, value) {
  return $store(ptr + 0, value)
}

function m_mapCreate() {
  return (() => {
    let ptr = c_Map()
    s_Map_items(ptr, m_listCreateDefault())
    return ptr
  })()
}
function m_mapClone(m_map) {
  return (() => {
    let ptr = c_Map()
    s_Map_items(ptr, m_listClone(g_Map_items(m_map)))
    return ptr
  })()
}
function m_mapGetItemInternal(m_map, m_key) {
  let m_items = g_Map_items(m_map)
  let m_itemsSize = m_listSize(m_items)
  let m_i = 0
while (Math.trunc(m_i < m_itemsSize)) {
  let m_item = m_listGet(m_items, m_i)
  let m_itemKey = g_MapItem_key(m_item)
if (m_stringEquals(m_itemKey, m_key)) {
  return (() => {
    let ptr = c_Some()
    s_Some_value(ptr, m_item)
    return ptr
  })()
}
  m_i = Math.trunc(m_i + 1)
}
  return (() => {
    let ptr = c_None()
    return ptr
  })()
}
function m_mapGet(m_map, m_key) {
  let m_item = m_mapGetItemInternal(m_map, m_key)
  return (() => {
    let ptr = m_item
    const tag = $load(ptr)
    if (tag === 0) { // Some
      const m_value = g_Some_value(ptr)
      return (() => {
    let ptr = c_Some()
    s_Some_value(ptr, g_MapItem_value(m_value))
    return ptr
  })()
    }
    if (tag === 1) { // None
      return (() => {
    let ptr = c_None()
    return ptr
  })()
    }
  })()
}
function m_mapSet(m_map, m_key, m_newValue) {
  let m_item = m_mapGetItemInternal(m_map, m_key)
  let m_m = (() => {
    let ptr = m_item
    const tag = $load(ptr)
    if (tag === 0) { // Some
      const m_value = g_Some_value(ptr)
      return (() => {
  s_MapItem_value(m_value, m_newValue)
  })()
    }
    if (tag === 1) { // None
      return (() => {
  let m_newItem = (() => {
    let ptr = c_MapItem()
    s_MapItem_key(ptr, m_key)
    s_MapItem_value(ptr, m_newValue)
    return ptr
  })()
  m_listPush(g_Map_items(m_map), m_newItem)
  })()
    }
  })()
}
function m_mapSize(m_map) {
  let m_items = g_Map_items(m_map)
  return m_listSize(m_items)
}
function m_mapContains(m_map, m_key) {
  let m_item = m_mapGetItemInternal(m_map, m_key)
  return (() => {
    let ptr = m_item
    const tag = $load(ptr)
    if (tag === 0) { // Some
      return 1
    }
    if (tag === 1) { // None
      return 0
    }
  })()
}
function m_min(m_a, m_b) {
if (Math.trunc(m_a < m_b)) {
  return m_a
}
  return m_b
}
function m_max(m_a, m_b) {
if (Math.trunc(m_a > m_b)) {
  return m_a
}
  return m_b
}
function m_exp(m_base, m_exponent) {
  let m_result = 1
  let m_pow = 0
while (Math.trunc(m_pow < m_exponent)) {
  m_result = Math.trunc(m_result * m_base)
  m_pow = Math.trunc(m_pow + 1)
}
  return m_result
}
function m_abs(m_x) {
if (Math.trunc(m_x < 0)) {
  return Math.trunc(0 - m_x)
}
  return m_x
}
function m_gcd(m_a, m_b) {
if (Math.trunc(m_b === 0)) {
  return m_a
}
  return m_gcd(m_b, Math.trunc(m_a % m_b))
}
function m_gcdMany(m_xs) {
  let m_result = m_listGet(m_xs, 0)
const $c_x = m_xs
for (let $i = 0; $i < m_listSize($c_x); $i++) {
  const m_x = m_listGet($c_x, $i)
  const m_index = $i
if (Math.trunc(m_index > 0)) {
  m_result = m_gcd(m_result, m_x)
}
}
  return m_result
}
function m_lcm(m_a, m_b) {
  return Math.trunc(Math.trunc(m_a * m_b) / m_gcd(m_a, m_b))
}
function m_lcmMany(m_xs) {
  let m_result = m_listGet(m_xs, 0)
const $c_x = m_xs
for (let $i = 0; $i < m_listSize($c_x); $i++) {
  const m_x = m_listGet($c_x, $i)
  const m_index = $i
if (Math.trunc(m_index > 0)) {
  m_result = m_lcm(m_result, m_x)
}
}
  return m_result
}
function c_Vec2() {
  const ptr = $alloc(8)
  return ptr
}
function g_Vec2_x(ptr) {
  return $load(ptr + 0)
}
function s_Vec2_x(ptr, value) {
  return $store(ptr + 0, value)
}
function g_Vec2_y(ptr) {
  return $load(ptr + 4)
}
function s_Vec2_y(ptr, value) {
  return $store(ptr + 4, value)
}

function m_vec2Add(m_a, m_b) {
  return (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, Math.trunc(g_Vec2_x(m_a) + g_Vec2_x(m_b)))
    s_Vec2_y(ptr, Math.trunc(g_Vec2_y(m_a) + g_Vec2_y(m_b)))
    return ptr
  })()
}
function m_vec2IsAdjacent(m_a, m_b) {
  let m_dx = m_abs(Math.trunc(g_Vec2_x(m_a) - g_Vec2_x(m_b)))
  let m_dy = m_abs(Math.trunc(g_Vec2_y(m_a) - g_Vec2_y(m_b)))
  return Math.trunc(Math.trunc(m_dx <= 1) && Math.trunc(m_dy <= 1))
}
function m_vec2ToString(m_v) {
  return m_stringConcat(m_stringCreate(m_arrayFrom(284)), m_stringConcat(m_stringFromNumber(g_Vec2_x(m_v)), m_stringConcat(m_stringCreate(m_arrayFrom(292)), m_stringConcat(m_stringFromNumber(g_Vec2_y(m_v)), m_stringCreate(m_arrayFrom(304))))))
}
function c_Range() {
  const ptr = $alloc(8)
  return ptr
}
function g_Range_start(ptr) {
  return $load(ptr + 0)
}
function s_Range_start(ptr, value) {
  return $store(ptr + 0, value)
}
function g_Range_end(ptr) {
  return $load(ptr + 4)
}
function s_Range_end(ptr, value) {
  return $store(ptr + 4, value)
}

function m_rangeCreate(m_start, m_end) {
  return (() => {
    let ptr = c_Range()
    s_Range_start(ptr, m_start)
    s_Range_end(ptr, m_end)
    return ptr
  })()
}
function m_rangeToString(m_range) {
  return m_stringConcat(m_stringCreate(m_arrayFrom(312)), m_stringConcat(m_stringFromNumber(g_Range_start(m_range)), m_stringConcat(m_stringCreate(m_arrayFrom(320)), m_stringConcat(m_stringFromNumber(g_Range_end(m_range)), m_stringCreate(m_arrayFrom(332))))))
}
function m_rangeSize(m_range) {
  return Math.trunc(g_Range_end(m_range) - g_Range_start(m_range))
}
function m_rangeEmpty(m_range) {
  return Math.trunc(m_rangeSize(m_range) === 0)
}
function m_rangesOverlap(m_a, m_b) {
  return Math.trunc(Math.trunc(g_Range_end(m_a) > g_Range_start(m_b)) && Math.trunc(g_Range_start(m_a) < g_Range_end(m_b)))
}
function m_rangeIntersect(m_a, m_b) {
if (m_rangesOverlap(m_a, m_b)) {
  return m_rangeCreate(m_max(g_Range_start(m_a), g_Range_start(m_b)), m_min(g_Range_end(m_a), g_Range_end(m_b)))
}
  return m_rangeCreate(0, 0)
}
function m_rangeUnion(m_a, m_b) {
if (m_rangesOverlap(m_a, m_b)) {
  return m_rangeCreate(m_min(g_Range_start(m_a), g_Range_start(m_b)), m_max(g_Range_end(m_a), g_Range_end(m_b)))
}
  return m_rangeCreate(0, 0)
}
function m_rangeShift(m_offset, m_range) {
  return m_rangeCreate(Math.trunc(g_Range_start(m_range) + m_offset), Math.trunc(g_Range_end(m_range) + m_offset))
}
function m_vec2IsEqual(m_a, m_b) {
  return Math.trunc(Math.trunc(g_Vec2_x(m_a) === g_Vec2_x(m_b)) && Math.trunc(g_Vec2_y(m_a) === g_Vec2_y(m_b)))
}
function c_Connections() {
  const ptr = $alloc(16)
  return ptr
}
function g_Connections_north(ptr) {
  return $load(ptr + 0)
}
function s_Connections_north(ptr, value) {
  return $store(ptr + 0, value)
}
function g_Connections_east(ptr) {
  return $load(ptr + 4)
}
function s_Connections_east(ptr, value) {
  return $store(ptr + 4, value)
}
function g_Connections_south(ptr) {
  return $load(ptr + 8)
}
function s_Connections_south(ptr, value) {
  return $store(ptr + 8, value)
}
function g_Connections_west(ptr) {
  return $load(ptr + 12)
}
function s_Connections_west(ptr, value) {
  return $store(ptr + 12, value)
}

function c_Pipes() {
  const ptr = $alloc(12)
  return ptr
}
function g_Pipes_start(ptr) {
  return $load(ptr + 0)
}
function s_Pipes_start(ptr, value) {
  return $store(ptr + 0, value)
}
function g_Pipes_grid(ptr) {
  return $load(ptr + 4)
}
function s_Pipes_grid(ptr, value) {
  return $store(ptr + 4, value)
}
function g_Pipes_size(ptr) {
  return $load(ptr + 8)
}
function s_Pipes_size(ptr, value) {
  return $store(ptr + 8, value)
}

function m_charToConnections(m_char) {
if (Math.trunc(m_char === 46)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 0)
    s_Connections_east(ptr, 0)
    s_Connections_south(ptr, 0)
    s_Connections_west(ptr, 0)
    return ptr
  })()
}
if (Math.trunc(m_char === 124)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 1)
    s_Connections_east(ptr, 0)
    s_Connections_south(ptr, 1)
    s_Connections_west(ptr, 0)
    return ptr
  })()
}
if (Math.trunc(m_char === 45)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 0)
    s_Connections_east(ptr, 1)
    s_Connections_south(ptr, 0)
    s_Connections_west(ptr, 1)
    return ptr
  })()
}
if (Math.trunc(m_char === 76)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 1)
    s_Connections_east(ptr, 1)
    s_Connections_south(ptr, 0)
    s_Connections_west(ptr, 0)
    return ptr
  })()
}
if (Math.trunc(m_char === 74)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 1)
    s_Connections_east(ptr, 0)
    s_Connections_south(ptr, 0)
    s_Connections_west(ptr, 1)
    return ptr
  })()
}
if (Math.trunc(m_char === 55)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 0)
    s_Connections_east(ptr, 0)
    s_Connections_south(ptr, 1)
    s_Connections_west(ptr, 1)
    return ptr
  })()
}
if (Math.trunc(m_char === 70)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 0)
    s_Connections_east(ptr, 1)
    s_Connections_south(ptr, 1)
    s_Connections_west(ptr, 0)
    return ptr
  })()
}
if (Math.trunc(m_char === 83)) {
  return (() => {
    let ptr = c_Connections()
    s_Connections_north(ptr, 1)
    s_Connections_east(ptr, 1)
    s_Connections_south(ptr, 1)
    s_Connections_west(ptr, 1)
    return ptr
  })()
}
}
function m_parseInput(m_input) {
  let m_lines = m_stringToLines(m_stringTrim(m_input))
  let m_grid = m_listCreateDefault()
  let m_start = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, 0)
    s_Vec2_y(ptr, 0)
    return ptr
  })()
  let m_size = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, m_stringSize(m_listGet(m_lines, 0)))
    s_Vec2_y(ptr, m_listSize(m_lines))
    return ptr
  })()
const $c_line = m_lines
for (let $i = 0; $i < m_listSize($c_line); $i++) {
  const m_line = m_listGet($c_line, $i)
  const m_y = $i
  let m_row = m_listCreateDefault()
const $c_char = m_line
for (let $i = 0; $i < m_stringSize($c_char); $i++) {
  const m_char = m_stringGet($c_char, $i)
  const m_x = $i
  let m_connections = m_charToConnections(m_char)
  m_listPush(m_row, m_connections)
if (Math.trunc(m_char === 83)) {
  m_start = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, m_x)
    s_Vec2_y(ptr, m_y)
    return ptr
  })()
}
}
  m_listPush(m_grid, m_row)
}
  return (() => {
    let ptr = c_Pipes()
    s_Pipes_grid(ptr, m_grid)
    s_Pipes_start(ptr, m_start)
    s_Pipes_size(ptr, m_size)
    return ptr
  })()
}
function m_connectionsToChar(m_connections) {
if (Math.trunc(Math.trunc(!g_Connections_north(m_connections)) && Math.trunc(Math.trunc(!g_Connections_east(m_connections)) && Math.trunc(Math.trunc(!g_Connections_south(m_connections)) && Math.trunc(!g_Connections_west(m_connections)))))) {
  return 46
} else {
if (Math.trunc(g_Connections_north(m_connections) && Math.trunc(g_Connections_east(m_connections) && Math.trunc(g_Connections_south(m_connections) && g_Connections_west(m_connections))))) {
  return 83
} else {
if (Math.trunc(g_Connections_north(m_connections) && g_Connections_south(m_connections))) {
  return 9553
} else {
if (Math.trunc(g_Connections_east(m_connections) && g_Connections_west(m_connections))) {
  return 9552
} else {
if (Math.trunc(g_Connections_south(m_connections) && g_Connections_east(m_connections))) {
  return 9556
} else {
if (Math.trunc(g_Connections_west(m_connections) && g_Connections_south(m_connections))) {
  return 9559
} else {
if (Math.trunc(g_Connections_north(m_connections) && g_Connections_west(m_connections))) {
  return 9565
} else {
if (Math.trunc(g_Connections_east(m_connections) && g_Connections_north(m_connections))) {
  return 9562
}
}
}
}
}
}
}
}
  return 32
}
function m_connectionsToWalls(m_connections) {
  let m_walls = m_listCreateDefault()
  let m_char = m_connectionsToChar(m_connections)
if (Math.trunc(m_char === 46)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
if (Math.trunc(m_char === 83)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 1)
    m_listPush(list, 1)
    m_listPush(list, 1)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
if (Math.trunc(m_char === 9553)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
if (Math.trunc(m_char === 9552)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 1)
    m_listPush(list, 1)
    m_listPush(list, 1)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
if (Math.trunc(m_char === 9556)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 1)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
if (Math.trunc(m_char === 9559)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 1)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
if (Math.trunc(m_char === 9565)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 1)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
if (Math.trunc(m_char === 9562)) {
  m_listPushList(m_walls, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 0)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 1)
    m_listPush(list, 1)
    return list
  })())
    m_listPush(list, (() => {
    let list = m_listCreateDefault()
    m_listPush(list, 0)
    m_listPush(list, 0)
    m_listPush(list, 0)
    return list
  })())
    return list
  })())
}
  return m_walls
}
function m_printPipes(m_grid) {
  let m_out = m_stringBuilderCreate(1024)
const $c_row = m_grid
for (let $i = 0; $i < m_listSize($c_row); $i++) {
  const m_row = m_listGet($c_row, $i)
const $c_connections = m_row
for (let $i = 0; $i < m_listSize($c_connections); $i++) {
  const m_connections = m_listGet($c_connections, $i)
  m_stringBuilderAppend(m_out, m_charToString(m_connectionsToChar(m_connections)))
}
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(340)))
}
  m_print(m_stringBuilderBuild(m_out))
}
function m_getConnections(m_pipes, m_p) {
  return m_listGet(m_listGet(g_Pipes_grid(m_pipes), g_Vec2_y(m_p)), g_Vec2_x(m_p))
}
function m_pointsContain(m_points, m_point) {
const $c_other = m_points
for (let $i = 0; $i < m_listSize($c_other); $i++) {
  const m_other = m_listGet($c_other, $i)
if (m_vec2IsEqual(m_point, m_other)) {
  return 1
}
}
  return 0
}
function m_formatConnections(m_connections) {
  let m_out = m_stringBuilderCreate(16)
if (g_Connections_north(m_connections)) {
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(348)))
}
if (g_Connections_east(m_connections)) {
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(360)))
}
if (g_Connections_south(m_connections)) {
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(372)))
}
if (g_Connections_west(m_connections)) {
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(384)))
}
  return m_stringBuilderBuild(m_out)
}
function m_searchPipes(m_pipes, m_visited, m_current, m_steps) {
if (m_pointsContain(m_visited, m_current)) {
  return m_steps
}
  let m_currentConnections = m_getConnections(m_pipes, m_current)







  m_listPush(m_visited, m_current)
  let m_size = g_Pipes_size(m_pipes)
  let m_maxSteps = m_steps
if (Math.trunc(g_Vec2_y(m_current) > 0)) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, g_Vec2_x(m_current))
    s_Vec2_y(ptr, Math.trunc(g_Vec2_y(m_current) - 1))
    return ptr
  })()
  let m_candidateConnections = m_getConnections(m_pipes, m_candidate)
if (Math.trunc(g_Connections_north(m_currentConnections) && g_Connections_south(m_candidateConnections))) {
  let m_nextSteps = m_searchPipes(m_pipes, m_visited, m_candidate, Math.trunc(m_steps + 1))
  m_maxSteps = m_max(m_maxSteps, m_nextSteps)
}
}
if (Math.trunc(g_Vec2_x(m_current) < Math.trunc(g_Vec2_x(m_size) - 1))) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, Math.trunc(g_Vec2_x(m_current) + 1))
    s_Vec2_y(ptr, g_Vec2_y(m_current))
    return ptr
  })()
  let m_candidateConnections = m_getConnections(m_pipes, m_candidate)
if (Math.trunc(g_Connections_east(m_currentConnections) && g_Connections_west(m_candidateConnections))) {
  let m_nextSteps = m_searchPipes(m_pipes, m_visited, m_candidate, Math.trunc(m_steps + 1))
  m_maxSteps = m_max(m_maxSteps, m_nextSteps)
}
}
if (Math.trunc(g_Vec2_y(m_current) < Math.trunc(g_Vec2_y(m_size) - 1))) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, g_Vec2_x(m_current))
    s_Vec2_y(ptr, Math.trunc(g_Vec2_y(m_current) + 1))
    return ptr
  })()
  let m_candidateConnections = m_getConnections(m_pipes, m_candidate)
if (Math.trunc(g_Connections_south(m_currentConnections) && g_Connections_north(m_candidateConnections))) {
  let m_nextSteps = m_searchPipes(m_pipes, m_visited, m_candidate, Math.trunc(m_steps + 1))
  m_maxSteps = m_max(m_maxSteps, m_nextSteps)
}
}
if (Math.trunc(g_Vec2_x(m_current) > 0)) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, Math.trunc(g_Vec2_x(m_current) - 1))
    s_Vec2_y(ptr, g_Vec2_y(m_current))
    return ptr
  })()
  let m_candidateConnections = m_getConnections(m_pipes, m_candidate)
if (Math.trunc(g_Connections_west(m_currentConnections) && g_Connections_east(m_candidateConnections))) {
  let m_nextSteps = m_searchPipes(m_pipes, m_visited, m_candidate, Math.trunc(m_steps + 1))
  m_maxSteps = m_max(m_maxSteps, m_nextSteps)
}
}
  return m_maxSteps
}
function m_solvePart1(m_input) {
  let m_pipes = m_parseInput(m_input)
  let m_steps = m_searchPipes(m_pipes, m_listCreateDefault(), g_Pipes_start(m_pipes), 0)
  let m_result = Math.trunc(m_steps / 2)
  return m_stringFromNumber(m_result)
}
function c_Lake() {
  const ptr = $alloc(8)
  return ptr
}
function g_Lake_walls(ptr) {
  return $load(ptr + 0)
}
function s_Lake_walls(ptr, value) {
  return $store(ptr + 0, value)
}
function g_Lake_size(ptr) {
  return $load(ptr + 4)
}
function s_Lake_size(ptr, value) {
  return $store(ptr + 4, value)
}

function m_buildLake(m_pipes, m_visited) {
  let m_walls = m_listCreateDefault()
  let m_pipesSize = g_Pipes_size(m_pipes)
  let m_lakeSize = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, Math.trunc(g_Vec2_x(m_pipesSize) * 3))
    s_Vec2_y(ptr, Math.trunc(g_Vec2_y(m_pipesSize) * 3))
    return ptr
  })()
  let m_y = 0
while (Math.trunc(m_y < g_Vec2_y(m_lakeSize))) {
  let m_row = m_listCreateDefault()
  let m_x = 0
while (Math.trunc(m_x < g_Vec2_x(m_lakeSize))) {
  m_listPush(m_row, 0)
  m_x = Math.trunc(m_x + 1)
}
  m_listPush(m_walls, m_row)
  m_y = Math.trunc(m_y + 1)
}
const $c_p = m_visited
for (let $i = 0; $i < m_listSize($c_p); $i++) {
  const m_p = m_listGet($c_p, $i)
  let m_connections = m_getConnections(m_pipes, m_p)
  let m_connectionsWalls = m_connectionsToWalls(m_connections)
const $c_crow = m_connectionsWalls
for (let $i = 0; $i < m_listSize($c_crow); $i++) {
  const m_crow = m_listGet($c_crow, $i)
  const m_dy = $i
const $c_wall = m_crow
for (let $i = 0; $i < m_listSize($c_wall); $i++) {
  const m_wall = m_listGet($c_wall, $i)
  const m_dx = $i
if (Math.trunc(m_wall === 1)) {
  let m_row = m_listGet(m_walls, Math.trunc(Math.trunc(g_Vec2_y(m_p) * 3) + m_dy))
  m_listSet(m_row, Math.trunc(Math.trunc(g_Vec2_x(m_p) * 3) + m_dx), 1)
}
}
}
}
  return (() => {
    let ptr = c_Lake()
    s_Lake_walls(ptr, m_walls)
    s_Lake_size(ptr, m_lakeSize)
    return ptr
  })()
}
function m_printLake(m_lake) {
  let m_out = m_stringBuilderCreate(1024)
  let m_size = g_Lake_size(m_lake)
  let m_y = 0
while (Math.trunc(m_y < g_Vec2_y(m_size))) {
  let m_x = 0
while (Math.trunc(m_x < g_Vec2_x(m_size))) {
  let m_value = m_listGet(m_listGet(g_Lake_walls(m_lake), m_y), m_x)
if (Math.trunc(m_value === 1)) {
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(396)))
} else {
if (Math.trunc(m_value === 2)) {
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(404)))
} else {
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(412)))
}
}
  m_x = Math.trunc(m_x + 1)
}
  m_stringBuilderAppend(m_out, m_stringCreate(m_arrayFrom(420)))
  m_y = Math.trunc(m_y + 1)
}
  m_print(m_stringBuilderBuild(m_out))
}
function m_floodLake(m_lake, m_current) {
  let m_currentRow = m_listGet(g_Lake_walls(m_lake), g_Vec2_y(m_current))
  let m_value = m_listGet(m_currentRow, g_Vec2_x(m_current))
  let m_size = g_Lake_size(m_lake)
if (Math.trunc(m_value !== 0)) {
  return 0
}
  m_listSet(m_currentRow, g_Vec2_x(m_current), 2)
if (Math.trunc(g_Vec2_y(m_current) > 0)) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, g_Vec2_x(m_current))
    s_Vec2_y(ptr, Math.trunc(g_Vec2_y(m_current) - 1))
    return ptr
  })()
  m_floodLake(m_lake, m_candidate)
}
if (Math.trunc(g_Vec2_x(m_current) < Math.trunc(g_Vec2_x(m_size) - 1))) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, Math.trunc(g_Vec2_x(m_current) + 1))
    s_Vec2_y(ptr, g_Vec2_y(m_current))
    return ptr
  })()
  m_floodLake(m_lake, m_candidate)
}
if (Math.trunc(g_Vec2_y(m_current) < Math.trunc(g_Vec2_y(m_size) - 1))) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, g_Vec2_x(m_current))
    s_Vec2_y(ptr, Math.trunc(g_Vec2_y(m_current) + 1))
    return ptr
  })()
  m_floodLake(m_lake, m_candidate)
}
if (Math.trunc(g_Vec2_x(m_current) > 0)) {
  let m_candidate = (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, Math.trunc(g_Vec2_x(m_current) - 1))
    s_Vec2_y(ptr, g_Vec2_y(m_current))
    return ptr
  })()
  m_floodLake(m_lake, m_candidate)
}
  return 0
}
function m_solvePart2(m_input) {
  let m_pipes = m_parseInput(m_input)
  let m_pipesSize = g_Pipes_size(m_pipes)
  let m_visited = m_listCreateDefault()
  let m_steps = m_searchPipes(m_pipes, m_visited, g_Pipes_start(m_pipes), 0)
  let m_lake = m_buildLake(m_pipes, m_visited)
  let m_count = m_floodLake(m_lake, (() => {
    let ptr = c_Vec2()
    s_Vec2_x(ptr, 0)
    s_Vec2_y(ptr, 0)
    return ptr
  })())
  let m_result = 0
  let m_y = 0
while (Math.trunc(m_y < g_Vec2_y(m_pipesSize))) {
  let m_x = 0
while (Math.trunc(m_x < g_Vec2_x(m_pipesSize))) {
  let m_lx = Math.trunc(Math.trunc(m_x * 3) + 1)
  let m_ly = Math.trunc(Math.trunc(m_y * 3) + 1)
  let m_lv = m_listGet(m_listGet(g_Lake_walls(m_lake), m_ly), m_lx)
if (Math.trunc(m_lv === 0)) {
  m_result = Math.trunc(m_result + 1)
}
  m_x = Math.trunc(m_x + 1)
}
  m_y = Math.trunc(m_y + 1)
}
  return m_stringFromNumber(m_result)
}
function m_main() {
  let m_input = m_fileRead(m_stringCreate(m_arrayFrom(428)))
  let m_part1 = m_solvePart1(m_input)
  m_print(m_stringCreate(m_arrayFrom(468)))
  m_print(m_part1)
  m_assertEqualString(m_part1, m_stringCreate(m_arrayFrom(500)))
  let m_part2 = m_solvePart2(m_input)
  m_print(m_stringCreate(m_arrayFrom(520)))
  m_print(m_part2)
  m_assertEqualString(m_part2, m_stringCreate(m_arrayFrom(552)))
}

const _args = process.argv.slice(2)
const _argsArray = m_arrayCreate(_args.length)
_args.forEach((arg, index) => {
m_arraySet(_argsArray, index, _stringToPointer(arg))
})
m_main(_argsArray)
process.env.COMPILER_DEBUG && console.log('Used bytes: ' + _memBase)