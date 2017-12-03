const distance = to => {
  const largestRoot = Math.floor(Math.sqrt(to));
  const remainingSquares = to - largestRoot * largestRoot;
  const even = largestRoot % 2 === 0;
  const sign = even ? -1 : 1;

  const squareX = even ? -Math.floor((largestRoot - 1) / 2) : Math.floor(largestRoot / 2);
  const squareY = even ? -squareX + 1 : -squareX;

  const extraX = remainingSquares > 0 ? sign - sign * Math.max(0, remainingSquares - (largestRoot + 1)) : 0;
  const extraY = remainingSquares > 0 ? sign * Math.min(remainingSquares - 1, largestRoot) : 0;

  return Math.abs(squareX + extraX) + Math.abs(squareY + extraY);
};

module.exports = distance;
