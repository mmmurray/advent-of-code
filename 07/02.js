const input = require('fs').readFileSync(`${__dirname}/input.txt`, 'utf-8');

const parseMatches = ([_, name, weight, holding]) => ({
  name,
  weight: Number(weight),
  holding: holding ? holding.split(', ') : []
});

const parseRow = row => parseMatches(/^([a-z]+) \((\d+)\)(?: -> (.+)$)?/.exec(row));

const programs = input
  .trim()
  .split('\n')
  .map(parseRow);

const bottomProgram = programs.find(p1 => !programs.find(p2 => p2.holding.indexOf(p1.name) !== -1));

const getHolding = (holder, arr) => holder.holding.map(name => arr.find(p3 => p3.name === name));

const getTotalWeight = p1 => p1.weight + getHolding(p1, programs).reduce((acc, p2) => acc + getTotalWeight(p2), 0);

const programsWithTotalWeights = programs.map(p1 => ({ ...p1, totalWeight: getTotalWeight(p1) }));

const getOddWeight = p => p.find(p1 => p.find(p2 => p1.totalWeight !== p2.totalWeight && p1 !== p2));

const isBalanced = p => new Set(getHolding(p, programsWithTotalWeights).map(h => h.totalWeight)).size === 1;

const findUnbalanced = p => {
  const holding = getHolding(p, programsWithTotalWeights);
  const oddOneOut = getOddWeight(holding);

  if (isBalanced(oddOneOut)) {
    const targetTotal = holding.find(h => h !== oddOneOut).totalWeight;
    const newWeight = oddOneOut.weight + (targetTotal - oddOneOut.totalWeight);
    return newWeight;
  }

  return findUnbalanced(oddOneOut);
};

console.log(findUnbalanced(bottomProgram));
