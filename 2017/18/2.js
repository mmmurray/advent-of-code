const process = (instructions, id, queue, send) => {
  const registers = { p: id };

  let instructionPointer = 0;

  const getValue = ptr => (typeof ptr === 'number' ? ptr : registers[ptr] || 0);

  return () => {
    if (instructionPointer < 0 || instructionPointer >= instructions.length) {
      return null;
    }

    const instruction = instructions[instructionPointer];

    if (instruction[0] === 'snd') {
      send(getValue(instruction[1]));
    } else if (instruction[0] === 'set') {
      registers[instruction[1]] = getValue(instruction[2]);
    } else if (instruction[0] === 'add') {
      registers[instruction[1]] = getValue(instruction[1]) + getValue(instruction[2]);
    } else if (instruction[0] === 'mul') {
      registers[instruction[1]] = getValue(instruction[1]) * getValue(instruction[2]);
    } else if (instruction[0] === 'mod') {
      registers[instruction[1]] = getValue(instruction[1]) % getValue(instruction[2]);
    } else if (instruction[0] === 'jgz') {
      instructionPointer += getValue(instruction[1]) > 0 ? getValue(instruction[2]) - 1 : 0;
    } else if (instruction[0] === 'rcv') {
      if (queue.length > 0) {
        registers[instruction[1]] = queue.shift();
      } else {
        return true;
      }
    }

    instructionPointer++;
  };
};

const execute = rawInstructions => {
  let numberOfSentTo1 = 0;

  const toNumber = n => (Number.isNaN(Number(n)) ? n : Number(n));

  const instructions = rawInstructions
    .trim()
    .split('\n')
    .map(i => i.split(' '))
    .map(i => [i[0], toNumber(i[1]), toNumber(i[2])]);

  const queue1 = [];
  const queue2 = [];

  const send1 = v => {
    queue2.push(v);
  };

  const send2 = v => {
    numberOfSentTo1++;
    queue1.push(v);
  };

  const tick1 = process(instructions, 0, queue1, send1);
  const tick2 = process(instructions, 1, queue2, send2);

  let running1 = true;
  let running2 = true;

  while (running1 || running2) {
    const res1 = tick1();
    if (res1 === null) {
      running1 = false;
    }

    const res2 = tick2();
    if (res2 === null) {
      running2 = false;
    }

    if (res1 && res2) {
      running1 = false;
      running2 = false;
    }
  }

  return numberOfSentTo1;
};

module.exports = execute;
