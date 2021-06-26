#!/usr/bin/lua

do
local _ENV = _ENV
package.preload[ "json" ] = function( ... ) local arg = _G.arg;
--
-- json.lua
--
-- Copyright (c) 2020 rxi
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy of
-- this software and associated documentation files (the "Software"), to deal in
-- the Software without restriction, including without limitation the rights to
-- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is furnished to do
-- so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--

local json = { _version = "0.1.2" }

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

local encode

local escape_char_map = {
  [ "\\" ] = "\\",
  [ "\"" ] = "\"",
  [ "\b" ] = "b",
  [ "\f" ] = "f",
  [ "\n" ] = "n",
  [ "\r" ] = "r",
  [ "\t" ] = "t",
}

local escape_char_map_inv = { [ "/" ] = "/" }
for k, v in pairs(escape_char_map) do
  escape_char_map_inv[v] = k
end


local function escape_char(c)
  return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte()))
end


local function encode_nil(val)
  return "null"
end


local function encode_table(val, stack)
  local res = {}
  stack = stack or {}

  -- Circular reference?
  if stack[val] then error("circular reference") end

  stack[val] = true

  if rawget(val, 1) ~= nil or next(val) == nil then
    -- Treat as array -- check keys are valid and it is not sparse
    local n = 0
    for k in pairs(val) do
      if type(k) ~= "number" then
        error("invalid table: mixed or invalid key types")
      end
      n = n + 1
    end
    if n ~= #val then
      error("invalid table: sparse array")
    end
    -- Encode
    for i, v in ipairs(val) do
      table.insert(res, encode(v, stack))
    end
    stack[val] = nil
    return "[" .. table.concat(res, ",") .. "]"

  else
    -- Treat as an object
    for k, v in pairs(val) do
      if type(k) ~= "string" then
        error("invalid table: mixed or invalid key types")
      end
      table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))
    end
    stack[val] = nil
    return "{" .. table.concat(res, ",") .. "}"
  end
end


local function encode_string(val)
  return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end


local function encode_number(val)
  -- Check for NaN, -inf and inf
  if val ~= val or val <= -math.huge or val >= math.huge then
    error("unexpected number value '" .. tostring(val) .. "'")
  end
  return string.format("%.14g", val)
end


local type_func_map = {
  [ "nil"     ] = encode_nil,
  [ "table"   ] = encode_table,
  [ "string"  ] = encode_string,
  [ "number"  ] = encode_number,
  [ "boolean" ] = tostring,
}


encode = function(val, stack)
  local t = type(val)
  local f = type_func_map[t]
  if f then
    return f(val, stack)
  end
  error("unexpected type '" .. t .. "'")
end


function json.encode(val)
  return ( encode(val) )
end


-------------------------------------------------------------------------------
-- Decode
-------------------------------------------------------------------------------

local parse

local function create_set(...)
  local res = {}
  for i = 1, select("#", ...) do
    res[ select(i, ...) ] = true
  end
  return res
end

local space_chars   = create_set(" ", "\t", "\r", "\n")
local delim_chars   = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals      = create_set("true", "false", "null")

local literal_map = {
  [ "true"  ] = true,
  [ "false" ] = false,
  [ "null"  ] = nil,
}


local function next_char(str, idx, set, negate)
  for i = idx, #str do
    if set[str:sub(i, i)] ~= negate then
      return i
    end
  end
  return #str + 1
end


local function decode_error(str, idx, msg)
  local line_count = 1
  local col_count = 1
  for i = 1, idx - 1 do
    col_count = col_count + 1
    if str:sub(i, i) == "\n" then
      line_count = line_count + 1
      col_count = 1
    end
  end
  error( string.format("%s at line %d col %d", msg, line_count, col_count) )
end


local function codepoint_to_utf8(n)
  -- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
  local f = math.floor
  if n <= 0x7f then
    return string.char(n)
  elseif n <= 0x7ff then
    return string.char(f(n / 64) + 192, n % 64 + 128)
  elseif n <= 0xffff then
    return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
  elseif n <= 0x10ffff then
    return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                       f(n % 4096 / 64) + 128, n % 64 + 128)
  end
  error( string.format("invalid unicode codepoint '%x'", n) )
end


local function parse_unicode_escape(s)
  local n1 = tonumber( s:sub(1, 4),  16 )
  local n2 = tonumber( s:sub(7, 10), 16 )
   -- Surrogate pair?
  if n2 then
    return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
  else
    return codepoint_to_utf8(n1)
  end
end


local function parse_string(str, i)
  local res = ""
  local j = i + 1
  local k = j

  while j <= #str do
    local x = str:byte(j)

    if x < 32 then
      decode_error(str, j, "control character in string")

    elseif x == 92 then -- `\`: Escape
      res = res .. str:sub(k, j - 1)
      j = j + 1
      local c = str:sub(j, j)
      if c == "u" then
        local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1)
                 or str:match("^%x%x%x%x", j + 1)
                 or decode_error(str, j - 1, "invalid unicode escape in string")
        res = res .. parse_unicode_escape(hex)
        j = j + #hex
      else
        if not escape_chars[c] then
          decode_error(str, j - 1, "invalid escape char '" .. c .. "' in string")
        end
        res = res .. escape_char_map_inv[c]
      end
      k = j + 1

    elseif x == 34 then -- `"`: End of string
      res = res .. str:sub(k, j - 1)
      return res, j + 1
    end

    j = j + 1
  end

  decode_error(str, i, "expected closing quote for string")
end


local function parse_number(str, i)
  local x = next_char(str, i, delim_chars)
  local s = str:sub(i, x - 1)
  local n = tonumber(s)
  if not n then
    decode_error(str, i, "invalid number '" .. s .. "'")
  end
  return n, x
end


local function parse_literal(str, i)
  local x = next_char(str, i, delim_chars)
  local word = str:sub(i, x - 1)
  if not literals[word] then
    decode_error(str, i, "invalid literal '" .. word .. "'")
  end
  return literal_map[word], x
end


local function parse_array(str, i)
  local res = {}
  local n = 1
  i = i + 1
  while 1 do
    local x
    i = next_char(str, i, space_chars, true)
    -- Empty / end of array?
    if str:sub(i, i) == "]" then
      i = i + 1
      break
    end
    -- Read token
    x, i = parse(str, i)
    res[n] = x
    n = n + 1
    -- Next token
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "]" then break end
    if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
  end
  return res, i
end


local function parse_object(str, i)
  local res = {}
  i = i + 1
  while 1 do
    local key, val
    i = next_char(str, i, space_chars, true)
    -- Empty / end of object?
    if str:sub(i, i) == "}" then
      i = i + 1
      break
    end
    -- Read key
    if str:sub(i, i) ~= '"' then
      decode_error(str, i, "expected string for key")
    end
    key, i = parse(str, i)
    -- Read ':' delimiter
    i = next_char(str, i, space_chars, true)
    if str:sub(i, i) ~= ":" then
      decode_error(str, i, "expected ':' after key")
    end
    i = next_char(str, i + 1, space_chars, true)
    -- Read value
    val, i = parse(str, i)
    -- Set
    res[key] = val
    -- Next token
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "}" then break end
    if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
  end
  return res, i
end


local char_func_map = {
  [ '"' ] = parse_string,
  [ "0" ] = parse_number,
  [ "1" ] = parse_number,
  [ "2" ] = parse_number,
  [ "3" ] = parse_number,
  [ "4" ] = parse_number,
  [ "5" ] = parse_number,
  [ "6" ] = parse_number,
  [ "7" ] = parse_number,
  [ "8" ] = parse_number,
  [ "9" ] = parse_number,
  [ "-" ] = parse_number,
  [ "t" ] = parse_literal,
  [ "f" ] = parse_literal,
  [ "n" ] = parse_literal,
  [ "[" ] = parse_array,
  [ "{" ] = parse_object,
}


parse = function(str, idx)
  local chr = str:sub(idx, idx)
  local f = char_func_map[chr]
  if f then
    return f(str, idx)
  end
  decode_error(str, idx, "unexpected character '" .. chr .. "'")
end


function json.decode(str)
  if type(str) ~= "string" then
    error("expected argument of type string, got " .. type(str))
  end
  local res, idx = parse(str, next_char(str, 1, space_chars, true))
  idx = next_char(str, idx, space_chars, true)
  if idx <= #str then
    decode_error(str, idx, "trailing garbage")
  end
  return res
end


return json
end
end

do
local _ENV = _ENV
package.preload[ "log" ] = function( ... ) local arg = _G.arg;
--
-- log.lua
--
-- Copyright (c) 2016 rxi
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the MIT license. See LICENSE for details.
--

local log = { _version = "0.1.0" }

log.usecolor = true
log.outfile = nil
log.level = "debug"


local modes = {
  { name = "debug",   color = "\27[36m", prefix = "DEBUG" },
  { name = "success", color = "\27[32m", prefix = "+" },
  { name = "failure",   color = "\27[31m", prefix = "-" },
  { name = "info",    color = "\27[32m", prefix = "*" },
  { name = "warn",    color = "\27[33m", prefix = "!" },
}


local levels = {}
for i, v in ipairs(modes) do
  levels[v.name] = i
end


local round = function(x, increment)
  increment = increment or 1
  x = x / increment
  return (x > 0 and math.floor(x + .5) or math.ceil(x - .5)) * increment
end


local _tostring = tostring

local tostring = function(...)
  local t = {}
  for i = 1, select('#', ...) do
    local x = select(i, ...)
    if type(x) == "number" then
      x = round(x, .01)
    end
    t[#t + 1] = _tostring(x)
  end
  return table.concat(t, " ")
end


for i, x in ipairs(modes) do
  local nameupper = x.name:upper()
  local prefix = x.prefix
  log[x.name] = function(...)
    
    -- Return early if we're below the log level
    if i < levels[log.level] then
      return
    end

    local msg = tostring(...)
    local info = debug.getinfo(2, "Sl")
    local lineinfo = info.short_src .. ":" .. info.currentline

    -- Output to console
    print(string.format("%s[%s][%s]%s %s",
                        log.usecolor and x.color or "",
                        prefix,
                        os.date("%H:%M:%S"),
                        log.usecolor and "\27[0m" or "",
                        msg))

    -- Output to log file
    if log.outfile then
      local fp = io.open(log.outfile, "a")
      local str = string.format("[%s][%s] %s: %s\n",
                                nameupper, os.date("%Y-%m-%d %H:%M:%S"), lineinfo, msg)
      fp:write(str)
      fp:close()
    end

  end
end


return log
end
end

do
local _ENV = _ENV
package.preload[ "md5" ] = function( ... ) local arg = _G.arg;
local md5 = {
  _VERSION     = "md5.lua 1.1.0",
  _DESCRIPTION = "MD5 computation in Lua (5.1-3, LuaJIT)",
  _URL         = "https://github.com/kikito/md5.lua",
  _LICENSE     = [[
    MIT LICENSE

    Copyright (c) 2013 Enrique García Cota + Adam Baldwin + hanzao + Equi 4 Software

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ]]
}

-- bit lib implementions

local char, byte, format, rep, sub =
  string.char, string.byte, string.format, string.rep, string.sub
local bit_or, bit_and, bit_not, bit_xor, bit_rshift, bit_lshift

local ok, bit = pcall(require, 'bit')
if ok then
  bit_or, bit_and, bit_not, bit_xor, bit_rshift, bit_lshift = bit.bor, bit.band, bit.bnot, bit.bxor, bit.rshift, bit.lshift
else
  ok, bit = pcall(require, 'bit32')

  if ok then

    bit_not = bit.bnot

    local tobit = function(n)
      return n <= 0x7fffffff and n or -(bit_not(n) + 1)
    end

    local normalize = function(f)
      return function(a,b) return tobit(f(tobit(a), tobit(b))) end
    end

    bit_or, bit_and, bit_xor = normalize(bit.bor), normalize(bit.band), normalize(bit.bxor)
    bit_rshift, bit_lshift = normalize(bit.rshift), normalize(bit.lshift)

  else

    local function tbl2number(tbl)
      local result = 0
      local power = 1
      for i = 1, #tbl do
        result = result + tbl[i] * power
        power = power * 2
      end
      return result
    end

    local function expand(t1, t2)
      local big, small = t1, t2
      if(#big < #small) then
        big, small = small, big
      end
      -- expand small
      for i = #small + 1, #big do
        small[i] = 0
      end
    end

    local to_bits -- needs to be declared before bit_not

    bit_not = function(n)
      local tbl = to_bits(n)
      local size = math.max(#tbl, 32)
      for i = 1, size do
        if(tbl[i] == 1) then
          tbl[i] = 0
        else
          tbl[i] = 1
        end
      end
      return tbl2number(tbl)
    end

    -- defined as local above
    to_bits = function (n)
      if(n < 0) then
        -- negative
        return to_bits(bit_not(math.abs(n)) + 1)
      end
      -- to bits table
      local tbl = {}
      local cnt = 1
      local last
      while n > 0 do
        last      = n % 2
        tbl[cnt]  = last
        n         = (n-last)/2
        cnt       = cnt + 1
      end

      return tbl
    end

    bit_or = function(m, n)
      local tbl_m = to_bits(m)
      local tbl_n = to_bits(n)
      expand(tbl_m, tbl_n)

      local tbl = {}
      for i = 1, #tbl_m do
        if(tbl_m[i]== 0 and tbl_n[i] == 0) then
          tbl[i] = 0
        else
          tbl[i] = 1
        end
      end

      return tbl2number(tbl)
    end

    bit_and = function(m, n)
      local tbl_m = to_bits(m)
      local tbl_n = to_bits(n)
      expand(tbl_m, tbl_n)

      local tbl = {}
      for i = 1, #tbl_m do
        if(tbl_m[i]== 0 or tbl_n[i] == 0) then
          tbl[i] = 0
        else
          tbl[i] = 1
        end
      end

      return tbl2number(tbl)
    end

    bit_xor = function(m, n)
      local tbl_m = to_bits(m)
      local tbl_n = to_bits(n)
      expand(tbl_m, tbl_n)

      local tbl = {}
      for i = 1, #tbl_m do
        if(tbl_m[i] ~= tbl_n[i]) then
          tbl[i] = 1
        else
          tbl[i] = 0
        end
      end

      return tbl2number(tbl)
    end

    bit_rshift = function(n, bits)
      local high_bit = 0
      if(n < 0) then
        -- negative
        n = bit_not(math.abs(n)) + 1
        high_bit = 0x80000000
      end

      local floor = math.floor

      for i=1, bits do
        n = n/2
        n = bit_or(floor(n), high_bit)
      end
      return floor(n)
    end

    bit_lshift = function(n, bits)
      if(n < 0) then
        -- negative
        n = bit_not(math.abs(n)) + 1
      end

      for i=1, bits do
        n = n*2
      end
      return bit_and(n, 0xFFFFFFFF)
    end
  end
end

-- convert little-endian 32-bit int to a 4-char string
local function lei2str(i)
  local f=function (s) return char( bit_and( bit_rshift(i, s), 255)) end
  return f(0)..f(8)..f(16)..f(24)
end

-- convert raw string to big-endian int
local function str2bei(s)
  local v=0
  for i=1, #s do
    v = v * 256 + byte(s, i)
  end
  return v
end

-- convert raw string to little-endian int
local function str2lei(s)
  local v=0
  for i = #s,1,-1 do
    v = v*256 + byte(s, i)
  end
  return v
end

-- cut up a string in little-endian ints of given size
local function cut_le_str(s,...)
  local o, r = 1, {}
  local args = {...}
  for i=1, #args do
    table.insert(r, str2lei(sub(s, o, o + args[i] - 1)))
    o = o + args[i]
  end
  return r
end

local swap = function (w) return str2bei(lei2str(w)) end

-- An MD5 mplementation in Lua, requires bitlib (hacked to use LuaBit from above, ugh)
-- 10/02/2001 jcw@equi4.com

local CONSTS = {
  0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
  0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
  0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
  0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
  0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
  0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
  0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
  0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
  0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
  0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
  0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
  0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
  0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
  0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
  0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
  0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
  0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476
}

local f=function (x,y,z) return bit_or(bit_and(x,y),bit_and(-x-1,z)) end
local g=function (x,y,z) return bit_or(bit_and(x,z),bit_and(y,-z-1)) end
local h=function (x,y,z) return bit_xor(x,bit_xor(y,z)) end
local i=function (x,y,z) return bit_xor(y,bit_or(x,-z-1)) end
local z=function (ff,a,b,c,d,x,s,ac)
  a=bit_and(a+ff(b,c,d)+x+ac,0xFFFFFFFF)
  -- be *very* careful that left shift does not cause rounding!
  return bit_or(bit_lshift(bit_and(a,bit_rshift(0xFFFFFFFF,s)),s),bit_rshift(a,32-s))+b
end

local function transform(A,B,C,D,X)
  local a,b,c,d=A,B,C,D
  local t=CONSTS

  a=z(f,a,b,c,d,X[ 0], 7,t[ 1])
  d=z(f,d,a,b,c,X[ 1],12,t[ 2])
  c=z(f,c,d,a,b,X[ 2],17,t[ 3])
  b=z(f,b,c,d,a,X[ 3],22,t[ 4])
  a=z(f,a,b,c,d,X[ 4], 7,t[ 5])
  d=z(f,d,a,b,c,X[ 5],12,t[ 6])
  c=z(f,c,d,a,b,X[ 6],17,t[ 7])
  b=z(f,b,c,d,a,X[ 7],22,t[ 8])
  a=z(f,a,b,c,d,X[ 8], 7,t[ 9])
  d=z(f,d,a,b,c,X[ 9],12,t[10])
  c=z(f,c,d,a,b,X[10],17,t[11])
  b=z(f,b,c,d,a,X[11],22,t[12])
  a=z(f,a,b,c,d,X[12], 7,t[13])
  d=z(f,d,a,b,c,X[13],12,t[14])
  c=z(f,c,d,a,b,X[14],17,t[15])
  b=z(f,b,c,d,a,X[15],22,t[16])

  a=z(g,a,b,c,d,X[ 1], 5,t[17])
  d=z(g,d,a,b,c,X[ 6], 9,t[18])
  c=z(g,c,d,a,b,X[11],14,t[19])
  b=z(g,b,c,d,a,X[ 0],20,t[20])
  a=z(g,a,b,c,d,X[ 5], 5,t[21])
  d=z(g,d,a,b,c,X[10], 9,t[22])
  c=z(g,c,d,a,b,X[15],14,t[23])
  b=z(g,b,c,d,a,X[ 4],20,t[24])
  a=z(g,a,b,c,d,X[ 9], 5,t[25])
  d=z(g,d,a,b,c,X[14], 9,t[26])
  c=z(g,c,d,a,b,X[ 3],14,t[27])
  b=z(g,b,c,d,a,X[ 8],20,t[28])
  a=z(g,a,b,c,d,X[13], 5,t[29])
  d=z(g,d,a,b,c,X[ 2], 9,t[30])
  c=z(g,c,d,a,b,X[ 7],14,t[31])
  b=z(g,b,c,d,a,X[12],20,t[32])

  a=z(h,a,b,c,d,X[ 5], 4,t[33])
  d=z(h,d,a,b,c,X[ 8],11,t[34])
  c=z(h,c,d,a,b,X[11],16,t[35])
  b=z(h,b,c,d,a,X[14],23,t[36])
  a=z(h,a,b,c,d,X[ 1], 4,t[37])
  d=z(h,d,a,b,c,X[ 4],11,t[38])
  c=z(h,c,d,a,b,X[ 7],16,t[39])
  b=z(h,b,c,d,a,X[10],23,t[40])
  a=z(h,a,b,c,d,X[13], 4,t[41])
  d=z(h,d,a,b,c,X[ 0],11,t[42])
  c=z(h,c,d,a,b,X[ 3],16,t[43])
  b=z(h,b,c,d,a,X[ 6],23,t[44])
  a=z(h,a,b,c,d,X[ 9], 4,t[45])
  d=z(h,d,a,b,c,X[12],11,t[46])
  c=z(h,c,d,a,b,X[15],16,t[47])
  b=z(h,b,c,d,a,X[ 2],23,t[48])

  a=z(i,a,b,c,d,X[ 0], 6,t[49])
  d=z(i,d,a,b,c,X[ 7],10,t[50])
  c=z(i,c,d,a,b,X[14],15,t[51])
  b=z(i,b,c,d,a,X[ 5],21,t[52])
  a=z(i,a,b,c,d,X[12], 6,t[53])
  d=z(i,d,a,b,c,X[ 3],10,t[54])
  c=z(i,c,d,a,b,X[10],15,t[55])
  b=z(i,b,c,d,a,X[ 1],21,t[56])
  a=z(i,a,b,c,d,X[ 8], 6,t[57])
  d=z(i,d,a,b,c,X[15],10,t[58])
  c=z(i,c,d,a,b,X[ 6],15,t[59])
  b=z(i,b,c,d,a,X[13],21,t[60])
  a=z(i,a,b,c,d,X[ 4], 6,t[61])
  d=z(i,d,a,b,c,X[11],10,t[62])
  c=z(i,c,d,a,b,X[ 2],15,t[63])
  b=z(i,b,c,d,a,X[ 9],21,t[64])

  return bit_and(A+a,0xFFFFFFFF),bit_and(B+b,0xFFFFFFFF),
         bit_and(C+c,0xFFFFFFFF),bit_and(D+d,0xFFFFFFFF)
end

----------------------------------------------------------------

local function md5_update(self, s)
  self.pos = self.pos + #s
  s = self.buf .. s
  for ii = 1, #s - 63, 64 do
    local X = cut_le_str(sub(s,ii,ii+63),4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)
    assert(#X == 16)
    X[0] = table.remove(X,1) -- zero based!
    self.a,self.b,self.c,self.d = transform(self.a,self.b,self.c,self.d,X)
  end
  self.buf = sub(s, math.floor(#s/64)*64 + 1, #s)
  return self
end

local function md5_finish(self)
  local msgLen = self.pos
  local padLen = 56 - msgLen % 64

  if msgLen % 64 > 56 then padLen = padLen + 64 end

  if padLen == 0 then padLen = 64 end

  local s = char(128) .. rep(char(0),padLen-1) .. lei2str(bit_and(8*msgLen, 0xFFFFFFFF)) .. lei2str(math.floor(msgLen/0x20000000))
  md5_update(self, s)

  assert(self.pos % 64 == 0)
  return lei2str(self.a) .. lei2str(self.b) .. lei2str(self.c) .. lei2str(self.d)
end

----------------------------------------------------------------

function md5.new()
  return { a = CONSTS[65], b = CONSTS[66], c = CONSTS[67], d = CONSTS[68],
           pos = 0,
           buf = '',
           update = md5_update,
           finish = md5_finish }
end

function md5.tohex(s)                                                               
  -- return format("%08x%08x%08x%08x", str2bei(sub(s, 1, 4)), str2bei(sub(s, 5, 8)), str2bei(sub(s, 9, 12)), str2bei(sub(s, 13, 16)))                                                
  local ret = ""                                                           
  local step = 2                                                 
  for i = 1, 16-step+1, step do                                        
      ret = ret..format("%04x", str2bei(sub(s, i, i+step-1)))          
  end                                                                 
  return ret                                                                                      
end

function md5.sum(s)
  return md5.new():update(s):finish()
end

function md5.sumhexa(s)
  return md5.tohex(md5.sum(s))
end

return md5
end
end

do
local _ENV = _ENV
package.preload[ "requests" ] = function( ... ) local arg = _G.arg;
local json = require "json"
local http = require "socket.http"
local ltn12 = require "ltn12"

http.TIMEOUT = 3

requests = {}

local function merge_table(first, second)
  for k, v in pairs(second) do
    first[k] = v
  end
end

function requests.get(reqt)
  local request_body = reqt.body
  local response_body = {}
  local request_headers = {
    ["User-Agent"] = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
    ["Cookie"] = reqt.cookie
  }
  
  if reqt.headers then
    merge_table(request_headers, reqt.headers)
  end
  
  local res, code, response_headers = http.request {
    url = reqt.url,
    method = "GET",
    headers = request_headers,
    source  =ltn12.source.string(request_body),
    sink = ltn12.sink.table(response_body),
    redirect = false
  }
  
  if type(response_headers) ~= "table" or type(response_body) ~= "table" then
    return nil
  end
  
  return code, response_headers, table.concat(response_body)
end

function requests.post(reqt)
  local request_body = reqt.body
  local response_body = {}
  local request_headers = {
    ["User-Agent"] = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
    ["Cookie"] = reqt.cookie
  }
  
  if reqt.headers then
    merge_table(request_headers, reqt.headers)
  end
  
  local res, code, response_headers = http.request {
    url = reqt.url,
    method = "POST",
    headers = request_headers,
    source  =ltn12.source.string(request_body),
    sink = ltn12.sink.table(response_body),
    redirect = false
  }
  
  if type(response_headers) ~= "table" or type(response_body) ~= "table" then
    return nil
  end
  
  return code, response_headers, table.concat(response_body)
end

function requests.json(reqt)
  local request_body = json.encode(reqt.body)
  local response_body = {}
  local request_headers = {
    ["User-Agent"] = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
    ["Accept"] = "*/*",
    ["Content-Type"] = "application/json",
    ["Content-Length"] = #request_body,
    ["Cookie"] = reqt.cookie
  }
  
  if reqt.headers then
    merge_table(request_headers, reqt.headers)
  end
  
  local res, code, response_headers = http.request {
    url = reqt.url,
    method = "POST",
    headers = request_headers,
    source  =ltn12.source.string(request_body),
    sink = ltn12.sink.table(response_body),
    redirect = false
  }
  
  if type(response_headers) ~= "table" or type(response_body) ~= "table" then
    return nil
  end
  
  return code, response_headers, json.decode(table.concat(response_body))
end
    
    
return requests
end
end

do
local _ENV = _ENV
package.preload[ "utils" ] = function( ... ) local arg = _G.arg;
local md5 = require "md5"
local socket = require "socket"

utils = {}

function utils.get_normal_authenticator(clientip, nasip, macaddr, timestamp, secretkey)
  return string.upper(md5.sumhexa(clientip .. nasip .. macaddr .. timestamp .. secretkey))
end

function utils.get_login_authenticator(clientip, nasip, macaddr, timestamp, vcode, secretkey)
  return string.upper(md5.sumhexa(clientip .. nasip .. macaddr .. timestamp .. vcode .. secretkey))
end

function utils.get_current_time()
  return os.date("%H:%M")
end

function utils.get_current_week()
  return tonumber(os.date("%w"))
end

local function parse_time(str)
  local hour, min = str:match("(%d+):(%d+)")
  return hour * 60 + min
end

function utils.is_time_between(time, start, stop)
  local _time = parse_time(time)
  local _start = parse_time(start)
  local _stop = parse_time(stop)
  
  if _stop < _start then
    return _start <= _time or _time <= _stop
  end
  return _start <= _time and _time <= _stop
end

function utils.is_array_contains(array, element)
  for _, value in ipairs(array) do
    if value == element then
      return true
    end
  end
  return false
end

function utils.sleep(sec)
  socket.select(nil, nil, sec)
end


return utils
end
end



local mime        = require "mime"

local requests    = require "requests"
local utils       = require "utils"
local log         = require "log"

log.outfile = "/tmp/esurfing-client/esurfing-client.log"
log.usecolor = false
log.level = "debug"

local unpack = unpack or table.unpack

-- 用户自定义参数 begin

-- 命令字符串 值只能是 "login" 或 "logout"
local command      = ""

-- 是否开启定时保活功能 默认开启 使用双路由器方法的用户无需开启
local keepalive_enabled = true
-- 保活协程检查周期(秒) 默认5分钟 实测应低于10分钟才不会断连
local keepalive_interval = 5 * 60

-- 免打扰模式 默认关闭
local donotdisturb = true
-- 免打扰模式启用的星期范围 [0 - 6 = 星期天 - 星期六]
-- 默认值适用于星期天到星期四晚上11点半断网, 第二天早上6点恢复
local dndweekrange = {0, 1, 2, 3, 4}
-- 免打扰模式开始时间(hh:mm)
local dndstarttime = "23:30"
-- 免打扰模式结束时间(hh:mm)
local dndstoptime  = "06:00"

local username     = ""
local password     = ""
local clientip     = ""
-- MAC地址格式为XX:XX:XX:XX:XX:XX
local macaddr      = ""
-- 以下三个变量对同一学校而言可认为是固定常数
local nasip        = "119.146.175.80"
local schoolid     = "1414"
local secretkey    = "Eshore!@#"

-- 用户自定义参数 end

local cookie       = ""
local vcode        = ""

function check_network_status()
  local code, _, _ = requests.get {
    url = "http://172.17.18.3:8080/portal/",
    headers = {
      ["Accept"] = "application/signed-exchange"
    }
  }
  
  if code == 200 or code == 302 then
    log.info("已连接到校园网")
    return true
  end
  
  log.failure("[-] 未连接到校园网, 请检查网络")
  return false
end

function portal_login()
  local code, _, _ = requests.post {
    url = "http://172.17.18.3:8080/portal/pws?t=li&ifEmailAuth=false",
    headers = {
      ["Accept"] = "application/signed-exchange"
    },
    body = string.format([[userName=%s&userPwd=%s]], username, mime.b64(password)) .. [[%3D&userDynamicPwd=&userDynamicPwdd=&serviceType=&isSavePwd=on&userurl=&userip=&basip=&language=Chinese&usermac=null&wlannasid=&wlanssid=&entrance=null&loginVerifyCode=&userDynamicPwddd=&customPageId=100&pwdMode=0&portalProxyIP=172.17.18.3&portalProxyPort=50200&dcPwdNeedEncrypt=1&assignIpType=0&appRootUrl=http%3A%2F%2F172.17.18.3%3A8080%2Fportal%2F&manualUrl=&manualUrlEncryptKey=]]
  }
  
  if code == 200 then
    log.info("已发包进行portal服务认证, 1秒后再次尝试登录")
    return true
  end
  
  log.failure("portal服务认证失败, 请检查网络")
  return false
end

function query_schoolid()
  local timestamp = os.time() * 1000
  local code, _, response_body = requests.json {
    url = "http://enet.10000.gd.cn:10001/client/queryschool",
    body = {
      clientip = clientip,
      nasip = nasip,
      mac = macaddr,
      timestamp = timestamp,
      authenticator = utils.get_normal_authenticator(clientip, nasip, macaddr, timestamp, secretkey)
    }
  }
  
  if code == 200 and response_body["rescode"] == "0" then
    schoolid = response_body["schoolid"]
    log.debug("schoolid: " .. schoolid)
    return true
  end
  
  log.failure("获取schoolid失败, 请检查网络")
  return false
end

function get_enet_cookie()
  local code, response_headers, _ = requests.get {
    url = "http://enet.10000.gd.cn:10001/advertisement.do",
    body = string.format([[schoolid=%s]], schoolid)
  }
  
  if code == 200 then
    cookie = response_headers["set-cookie"]
    log.debug("cookie: " .. cookie)
    return true
  end
  
  log.failure("获取认证cookie失败, 请检查网络")
  return false
end

function get_vcode()
  local timestamp = os.time() * 1000
  local code, _, response_body = requests.json {
    url = "http://enet.10000.gd.cn:10001/client/challenge",
    cookie = cookie,
    body = {
      username = username,
      clientip = clientip,
      nasip = nasip,
      mac = macaddr,
      timestamp = timestamp,
      authenticator = utils.get_normal_authenticator(clientip, nasip, macaddr, timestamp, secretkey)
    }
  }
  
  if code == 200 and response_body["rescode"] == "0" then
    vcode = response_body["challenge"]
    log.debug("vcode: " .. vcode)
    return true
  end
  
  log.failure("获取vcode失败, 请检查网络或确定登录信息是否正确")
  return false
end

function enet_login()
  local timestamp = os.time() * 1000
  local code, _, response_body = requests.json {
    url = "http://enet.10000.gd.cn:10001/client/login",
    cookie = cookie,
    body = {
      username = username,
      password = password,
      clientip = clientip,
      nasip = nasip,
      mac = macaddr,
      iswifi = "4060",
      timestamp = timestamp,
      authenticator = utils.get_login_authenticator(clientip, nasip, macaddr, timestamp, vcode, secretkey)
    }
  }
  
  if code == 200 and response_body["rescode"] == "0" then
    log.success("登录成功 - " .. response_body["resinfo"])
    return true
  end
  
  log.failure("登录失败, 请检查网络或确定登录信息是否正确")
  return false
end

function enet_logout()
  local timestamp = os.time() * 1000
  local code, _, response_body = requests.json {
    url = "http://enet.10000.gd.cn:10001/client/logout",
    cookie = cookie,
    body = {
      username = username,
      clientip = clientip,
      nasip = nasip,
      mac = macaddr,
      timestamp = timestamp,
      authenticator = utils.get_normal_authenticator(clientip, nasip, macaddr, timestamp, secretkey)
    }
  }
  
  if code == 200 and response_body["rescode"] == "0" then
    log.success("注销成功 - " .. response_body["resinfo"])
    return true
  end
  
  log.failure("注销失败, 请检查网络或确定登录信息是否正确")
  return false
end

function keepalive_coroutine()
  return coroutine.create(function()
    local i = 1
    while true do
      log.info("正在进行第" .. i .. "次保活操作")
      
      local status = check_network_status()
      
      if not status then
        return
      end
  
      login()
      
      coroutine.yield(status)
      utils.sleep(keepalive_interval)
      i = i + 1
    end
    
  end)
end

function keepalive()
  local co = keepalive_coroutine()
  repeat
    if donotdisturb then
      local current_week = utils.get_current_week()
      if utils.is_array_contains(dndweekrange, current_week) then
        local current_time = utils.get_current_time()
        if utils.is_time_between(current_time, dndstarttime, dndstoptime) then
          break
        end
      end
    end
    local _, status = coroutine.resume(co)
  until not status
end

function login()
  local code, response_headers, _ = requests.get {
    url = "http://www.qq.com"
  }
  
  if not code then
    log.failure("无法连接外网, 可能是退出登录后网络状态未刷新")
    return
  end

  if code == 302 then
    local location = response_headers["location"]
    if location == "https://www.qq.com/" then
      log.success("当前设备已登录")
      return
    end
    
    log.info("当前校园网环境为有线网络环境")
    
    if location:match("172.17.18.3:8080") then
      log.info("检测到需要portal服务认证")
      if portal_login() then
        login()
      end
      return
    end
    
    log.debug("获取到重定向地址为 " .. location)
    
    if clientip == "" or nasip == "" then
      clientip, nasip = location:match("wlanuserip=(.+)&wlanacip=(.+)")
    end

    log.debug("clientip: " .. clientip)
    log.debug("nasip: " .. nasip)
    log.debug("macaddr: " .. macaddr)
    
    if not query_schoolid() then
      return
    end
    
    if not get_enet_cookie() then
      return
    end
    
    if not get_vcode() then
      return
    end
    
    if not enet_login() then
      return
    end
    
    
  elseif code == 200 then
    log.info("当前校园网环境为无线WiFi环境")
    log.info("暂未实现该环境的登录, 请切换到有线宽带登录")
    return
  end
end

function help()
  print("<xxx>: 必选参数\n[xxx]: 可选项(一定要按照顺序)\n")
  print(
    [[Usage:
    main.lua login/logout <username> <password> <macaddr> [clientip] [nasip] [schoolid] [secretkey] - 登录/注销
    自行实现对应平台自动获取MAC地址的功能后:
    main.lua login <username> <password> - 简化登录
    main.lua logout <username> - 简化注销]]
  )
end

function main()
  print("======================")
  
  if donotdisturb then
    log.info("已开启免打扰模式")
    local current_week = utils.get_current_week()
    if utils.is_array_contains(dndweekrange, current_week) then
      local current_time = utils.get_current_time()
      if utils.is_time_between(current_time, dndstarttime, dndstoptime) then
        log.failure("免打扰时间段内脚本不会运行")
        return
      end
    end
    log.info("当前非免打扰时间段, 脚本将继续执行")
  end
  
  if #arg == 0 then
    if command == "" or username == "" then
      help()
      return
    end
  end
  
  if #arg == 1 or arg[1] == "-h" or arg[1] == "--help" then
    help()
    return
  end
  
  if #arg == 2 then
    command, username = unpack(arg)
    if command == "login" then
      log.failure("登录需要提供密码")
      return
    end
  elseif #arg == 3 then
    command, username, password = unpack(arg)
  elseif #arg == 4 then
    command, username, password, macaddr = unpack(arg)
  elseif #arg == 5 then
    command, username, password, macaddr, clientip = unpack(arg)
  elseif #arg == 6 then
    command, username, password, macaddr, clientip, nasip = unpack(arg)
  elseif #arg == 7 then
    command, username, password, macaddr, clientip, nasip, schoolid = unpack(arg)
  elseif #arg == 8 then
    command, username, password, macaddr, clientip, nasip, schoolid, secretkey = unpack(arg)
  end
  
  if macaddr == "" then
    local nwm = require "luci.model.network".init()
    local wandev = nwm:get_wandev()

    macaddr = wandev:mac():gsub(":", "-")

    if command == "logout" then
      clientip = wandev:get_network():ipaddr()
    end
  else
    macaddr = macaddr:gsub(":", "-")
  end
  
  local status = check_network_status()
  
  if not status then
    return
  end
  
  if command == "login" then
    log.info("正在尝试进行登录中...")
    login()
    if keepalive_enabled then
      keepalive()
    end
  elseif command == "logout" then
    log.info("正在尝试进行注销中...")
    enet_logout()
  end
end


main()
print("======================")