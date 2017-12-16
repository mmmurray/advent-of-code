const spin = (dancers, count) => {
  const [...newDancers] = dancers;
  while (count--) {
    newDancers.unshift(newDancers.pop());
  }
  return newDancers;
};

const exchange = (dancers, x, y) => {
  const [...newDancers] = dancers;
  const atX = newDancers[x];
  newDancers[x] = newDancers[y];
  newDancers[y] = atX;
  return newDancers;
};

const partner = (dancers, x, y) => {
  const [...newDancers] = dancers;
  const xIndex = dancers.indexOf(x);
  const yIndex = dancers.indexOf(y);
  newDancers[xIndex] = y;
  newDancers[yIndex] = x;
  return newDancers;
};

const dance = (dancers, moves) =>
  moves
    .trim()
    .split(',')
    .reduce((currentDancers, move) => {
      if (move[0] === 's') {
        return spin(currentDancers, Number(move.substr(1)));
      } else if (move[0] === 'x') {
        return exchange(
          currentDancers,
          ...move
            .substr(1)
            .split('/')
            .map(Number)
        );
      } else {
        return partner(currentDancers, move[1], move[3]);
      }
    }, dancers.split(''))
    .join('');

module.exports = dance;
