const spin = step => {
  const buffer = [0];
  let currentPosition = 0;

  for (let i = 0; i < 2017; i++) {
    const indexAfterStep = (currentPosition + step) % buffer.length;
    currentPosition = indexAfterStep + 1;
    buffer.splice(currentPosition, 0, i + 1);
  }

  return buffer[buffer.indexOf(2017) + 1];
};

module.exports = spin;
