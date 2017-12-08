const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');

const parseMatches = ([_, target, operation, value, test, conditional, threshold]) => ({
  target,
  operation: operation === 'inc' ? 1 : -1,
  value: Number(value),
  test,
  conditional,
  threshold: Number(threshold)
});

const parseInstruction = row => parseMatches(/^([a-z]+) (dec|inc) (-?\d+) if ([a-z]+) (.+) (-?\d+)$/.exec(row));

const processInstruction = (registers, instruction) => {
  const testValue = registers[instruction.test] || 0;
  const currentValue = registers[instruction.target] || 0;
  if (eval(`${testValue} ${instruction.conditional} ${instruction.threshold}`)) {
    return { ...registers, [instruction.target]: currentValue + instruction.operation * instruction.value };
  }
  return registers;
};

const getLargest = registers => Object.values(registers).sort((a, b) => b - a)[0];

const registers = input
  .trim()
  .split('\n')
  .map(parseInstruction)
  .reduce(processInstruction, {});

console.log(getLargest(registers));
