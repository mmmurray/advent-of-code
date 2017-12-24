const execute = () => {
  let b = 106700;
  let c = 123700;
  let h = 0;

  while (b <= c) {
    let f = false;
    for (let i = 2; i < b; i++) {
      if (b % i === 0) {
        f = true;
      }
    }
    if (f) {
      h++;
    }

    b += 17;
  }

  return h;
};

module.exports = execute;
