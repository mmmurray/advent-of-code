import { range } from '@common';

type Operation =
  | { $: 'add'; value: number }
  | { $: 'multiply'; value: number }
  | { $: 'square' };

type ThrowCondition = {
  divisibleBy: number;
  trueTarget: number;
  falseTarget: number;
};

type Monkey = {
  holding: number[];
  inspectOperation: Operation;
  throwCondition: ThrowCondition;
  inspectedCount: number;
};

const parseOperation = (input: string): Operation => {
  const addMatches = /old \+ (\d+)$/.exec(input);
  if (addMatches) {
    return { $: 'add', value: Number(addMatches[1]) };
  }

  const multiplyMatches = /old \* (\d+)$/.exec(input);
  if (multiplyMatches) {
    return { $: 'multiply', value: Number(multiplyMatches[1]) };
  }

  return { $: 'square' };
};

const getLastNumberOnLine = (line: string): number => {
  return Number(line.substring(line.lastIndexOf(' ') + 1));
};

const parseMonkey = (input: string): Monkey => {
  const [
    ,
    startingItems,
    inspectOperation,
    testCondition,
    trueTarget,
    falseTarget,
  ] = input.split('\n');

  const holding = startingItems
    .substring(startingItems.indexOf(':') + 1)
    .split(',')
    .map(Number);

  return {
    holding,
    inspectOperation: parseOperation(inspectOperation),
    throwCondition: {
      divisibleBy: getLastNumberOnLine(testCondition),
      trueTarget: getLastNumberOnLine(trueTarget),
      falseTarget: getLastNumberOnLine(falseTarget),
    },
    inspectedCount: 0,
  };
};

const parseInput = (input: string): Monkey[] => {
  return input.trim().split('\n\n').map(parseMonkey);
};

const applyOperation = (operation: Operation, value: number): number => {
  switch (operation.$) {
    case 'add': {
      return value + operation.value;
    }
    case 'multiply': {
      return value * operation.value;
    }
    case 'square': {
      return value * value;
    }
  }
};

const processMonkey = (
  reduceCondition: (x: number) => number,
  monkeys: Monkey[],
  current: number,
): Monkey[] => {
  const monkey = monkeys[current];

  const inspectedItems = monkey.holding.map<[number, number]>((item) => {
    const newItem = reduceCondition(
      applyOperation(monkey.inspectOperation, item),
    );
    const condition = newItem % monkey.throwCondition.divisibleBy === 0;
    const target = condition
      ? monkey.throwCondition.trueTarget
      : monkey.throwCondition.falseTarget;

    return [target, newItem];
  });

  const newMonkeys = monkeys.map((monkey, index) => {
    const receivedItems = inspectedItems
      .filter(([target]) => target === index)
      .map(([, item]) => item);

    if (index === current) {
      return {
        ...monkey,
        holding: [],
        inspectedCount: monkey.inspectedCount + inspectedItems.length,
      };
    }

    return {
      ...monkey,
      holding: [...monkey.holding, ...receivedItems],
    };
  });

  return newMonkeys;
};

const processRound = (
  reduceCondition: (x: number) => number,
  monkeys: Monkey[],
): Monkey[] => {
  return monkeys.reduce(
    (acc, _, index) => processMonkey(reduceCondition, acc, index),
    monkeys,
  );
};

const solve = (
  monkeysInitial: Monkey[],
  rounds: number,
  reduceCondition: (x: number) => number,
): number => {
  const monkeysFinal = range(0, rounds).reduce(
    (acc) => processRound(reduceCondition, acc),
    monkeysInitial,
  );

  const [first, second] = monkeysFinal
    .map((monkey) => monkey.inspectedCount)
    .sort((a, b) => b - a);

  return first * second;
};

export const solvePart1 = (input: string): number => {
  const monkeys = parseInput(input);
  const reduceCondition = (x: number) => Math.floor(x / 3);

  return solve(monkeys, 20, reduceCondition);
};

export const solvePart2 = (input: string): number => {
  const monkeys = parseInput(input);
  const lcm = monkeys.reduce(
    (acc, { throwCondition: { divisibleBy } }) => acc * divisibleBy,
    1,
  );
  const reduceCondition = (x: number) => x % lcm;

  return solve(monkeys, 10000, reduceCondition);
};
