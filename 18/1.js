const execute = rawInstructions => {
  const toNumber = n => (Number.isNaN(Number(n)) ? n : Number(n));

  const instructions = rawInstructions
    .trim()
    .split('\n')
    .map(i => i.split(' '))
    .map(i => [i[0], toNumber(i[1]), toNumber(i[2])]);

  const state = { registers: {}, lastFrequency: 0, firstRecovered: null };

  let instructionPointer = 0;
  let c = 0;

  const getValue = ptr => (typeof ptr === 'number' ? ptr : state.registers[ptr] || 0);

  while (true) {
    if (instructionPointer < 0 || instructionPointer >= instructions.length) {
      break;
    }

    const instruction = instructions[instructionPointer];

    if (instruction[0] === 'snd') {
      state.lastFrequency = getValue(instruction[1]);
    } else if (instruction[0] === 'set') {
      state.registers[instruction[1]] = getValue(instruction[2]);
    } else if (instruction[0] === 'add') {
      state.registers[instruction[1]] = getValue(instruction[1]) + getValue(instruction[2]);
    } else if (instruction[0] === 'mul') {
      state.registers[instruction[1]] = getValue(instruction[1]) * getValue(instruction[2]);
    } else if (instruction[0] === 'mod') {
      state.registers[instruction[1]] = getValue(instruction[1]) % getValue(instruction[2]);
    } else if (instruction[0] === 'jgz') {
      instructionPointer += getValue(instruction[1]) > 0 ? getValue(instruction[2]) - 1 : 0;
    } else if (instruction[0] === 'rcv') {
      if (state.registers[instruction[1]] !== 0 && state.firstRecovered === null) {
        state.firstRecovered = state.lastFrequency;
        break;
      }
    }

    instructionPointer++;
  }

  return state;
};

module.exports = execute;
