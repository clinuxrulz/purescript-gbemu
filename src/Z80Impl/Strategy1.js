// module Z80Impl.Strategy1

"use strict";

var unit = {};

var FLAGS_INDEX = 1;

exports.nop = function(z80State) {
  return function() {
    return unit;
  };
};

exports.ldReg16Imm16 = function(regIndex) {
  return function(imm16) {
    return function(z80State) {
      return function() {
        z80State.rs[regIndex] = imm16 >> 8;
        z80State.rs[regIndex + 1] = imm16 & 8;
        return unit;
      };
    };
  };
};

exports.ldMemReg16Reg8 = function(reg16Index) {
  return function(reg8Index) {
    return function(z80State) {
      return function() {
        z80State.mem[(z80State.rs[reg16Index] << 8) | z80State.rs[reg16Index + 1]] = z80State.rs[reg8Index];
      };
    };
  };
};

exports.incReg16 = function(reg16Index) {
  return function(z80State) {
    return function() {
      var x = z80State.rs[reg16Index + 1];
      x++;
      if (x & 0x100) { z80State.rs[reg16Index]++; }
      z80State.rs[reg16Index + 1] = x & 0xFF;
    };
  };
};

exports.incReg8 = function(reg8Index) {
  return function(z80State) {
    return function() {
      var x = z80State.rs[reg8Index];
      x++;
      var x2 = x & 0xFF;
      z80State.rs[reg8Index] = x2;
      z80State.rs[FLAGS_INDEX] = (x2 == 0 ? 0x80 : 0)
                               | (x & 0x100 ? 0x20 : 0)
                               | (z80State.rs[FLAGS_INDEX] & 0x10);
    };
  };
};

exports.decReg8 = function(reg8Index) {
  return function(z80State) {
    return function() {
      var x = z80State.rs[reg8Index];
      x--;
      var x2 = x & 0xFF;
      z80State.rs[reg8Index] = x2;
      z80State.rs[FLAGS_INDEX] = (x2 == 0 ? 0x80 : 0)
                               | 0x40
                               | (x & 0x100 ? 0x20 : 0)
                               | (z80State.rs[FLAGS_INDEX] & 0x10);
    };
  };
};
