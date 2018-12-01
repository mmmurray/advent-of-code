const spin = step => {
  let bufferLength = 1;
  let currentPosition = 0;
  let after0;

  for (let i = 0; i < 50000000; i++) {
    const indexAfterStep = (currentPosition + step) % bufferLength;
    currentPosition = indexAfterStep + 1;
    if (currentPosition === 1) {
      after0 = i + 1;
    }
    bufferLength++;
  }

  return after0;
};

module.exports = spin;
