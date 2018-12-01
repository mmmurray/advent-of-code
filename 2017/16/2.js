const spin = (dancers, count) => {
  const l = dancers.length - 1;
  while (count--) {
    dancers = [dancers[l], ...dancers.slice(0, l)];
  }
  return dancers;
};

const exchange = (dancers, x, y) => {
  const atX = dancers[x];
  dancers[x] = dancers[y];
  dancers[y] = atX;
  return dancers;
};

const partner = (dancers, x, y) => {
  const xIndex = dancers.indexOf(x);
  const yIndex = dancers.indexOf(y);
  dancers[xIndex] = y;
  dancers[yIndex] = x;
  return dancers;
};

const reducer = (currentDancers, move) => {
  if (move[0] === 0) {
    return spin(currentDancers, move[1]);
  } else if (move[0] === 1) {
    return exchange(currentDancers, move[1], move[2]);
  } else {
    return partner(currentDancers, move[1], move[2]);
  }
};

const dance = (dancers, moves) => {
  const parsedMoves = moves
    .trim()
    .split(',')
    .map(move => {
      const moveType = move[0] === 's' ? 0 : move[0] === 'x' ? 1 : 2;
      if (move[0] === 's') {
        return [0, Number(move.substr(1))];
      } else if (move[0] === 'x') {
        return [
          1,
          ...move
            .substr(1)
            .split('/')
            .map(Number)
        ];
      }
      return [2, move[1], move[3]];
    });

  let parsedDancers = dancers.split('');
  let cycleLength = 0;

  while (true) {
    parsedDancers = parsedMoves.reduce(reducer, parsedDancers);
    cycleLength++;
    if (parsedDancers.join('') === dancers) {
      break;
    }
  }

  const remainder = 1000000000 - Math.floor(1000000000 / cycleLength) * cycleLength;

  for (let i = 0; i < remainder; i++) {
    parsedDancers = parsedMoves.reduce(reducer, parsedDancers);
  }

  return parsedDancers.join('');
};

module.exports = dance;
