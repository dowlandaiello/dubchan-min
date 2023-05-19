export const loadCaptcha = (gun, app) => (p) => {
  const n = parseInt(p.hash.substring(p.hash.length - 2, p.hash.length), 16);
  captchaFor(gun, p, n, [], (chain) => {
    if (chain.length == 0)
      return;

    app.ports.loadedCaptcha.send({ post: p.hash, captcha: chain[n % chain.length].captcha });
  });
};

export const genCaptcha = (app) => async () => {
  const token = captcha();
  const canvas = document.getElementById('captchaGen');
  const parsed = { answer: token, data: canvas.toDataURL('image/jpeg') };
  app.ports.gotCaptcha.send(parsed);
};

const captchaFor = (gun, curr, n, chain, callback) => {
  if (n == 0) {
    callback(chain);
  }

  const analysis = (prevStr) => {
    if (prevStr === undefined) {
      callback(chain);

      return;
    }

    try {
      const prev = JSON.parse(prevStr);

      if (prev.captcha === undefined || prev.captcha.answer === undefined || prev.captcha.answer.length === 5 || prev.timestamp <= 1683831536) {
        callback(chain);

        return;
      }

      chain.push(prev);

      captchaFor(gun, prev, n - 1, chain, callback);
    } catch (e) {
      console.error(e);
    }
  };

  if (curr.prev === undefined && curr.parent !== undefined) {
    rootFor(gun, curr, (rootStr) => {
      try {
        const root = JSON.parse(rootStr);

        if (root.timestamp <= 1683831536) {
          gun.get('#posts').get('WpsHb7YL5nA8qeYRNCuJ6NmGIlCbMCkiZj0gowjJmXc=').once((alternateStr) => {
            try {
              const alternate = JSON.parse(alternateStr);
              captchaFor(gun, alternate, n, chain, callback);
            } catch (e) {
              console.error(e);
            }
          });


          return;
        }

        analysis(rootStr);
      } catch (e) {
        console.error(e);
      }
    });
  }

  gun.get('#posts').get(curr.prev).once(analysis);
};

const rootFor = (gun, curr, callback) => {
  gun.get('#comments').get(curr.parent).once((prevStr) => {
    try {
      const prev = JSON.parse(prevStr);

      rootFor(prev, callback);
    } catch (e) {
      console.error(e);
    }
  });

  gun.get('#posts').get(curr.parent).once((prevStr) => {
    callback(prevStr);
  });
};

const randomColor = () => {
  let r = Math.floor(Math.random()*256);
  let g = Math.floor(Math.random()*256);
  let b = Math.floor(Math.random()*256);
  return 'rgb(' + r + ',' + g + ',' + b + ')';
};

const captcha = () => {
  let showNum = [];
  let canvasWinth = 700;
  let canvasHeight = 146;
  let canvas = document.getElementById('captchaGen');
  let context = canvas.getContext('2d');
  canvas.width = canvasWinth;
  canvas.height = canvasHeight;
  let sCode = 'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,0,1,2,3,4,5,6,7,8,9,!,@,#,$,%,^,&,*,(,)';
  let saCode = sCode.split(',');
  let saCodeLen = saCode.length;
  for (let i = 0; i <= 3; i++) {
    let sIndex = Math.floor(Math.random()*saCodeLen);
    let sDeg = (Math.random()*30*Math.PI) / 180;
    let cTxt = saCode[sIndex];
    showNum[i] = cTxt.toLowerCase();
    let x = 200 + i*90;
    let y = 90 + Math.random()*8;
    context.font = 'bold 90px arial';
    context.translate(x, y);
    context.rotate(sDeg);

    context.fillStyle = randomColor();
    context.fillText(cTxt, 0, 0);

    context.rotate(-sDeg);
    context.translate(-x, -y);
  }
  for (let i = 0; i <= 5; i++) {
    context.strokeStyle = randomColor();
    context.beginPath();
    context.moveTo(
      Math.random() * canvasWinth,
      Math.random() * canvasHeight
    );
    context.lineTo(
      Math.random() * canvasWinth,
      Math.random() * canvasHeight
    );
    context.stroke();
  }
  for (let i = 0; i < 6000; i++) {
    context.strokeStyle = randomColor();
    context.beginPath();
    let x = Math.random() * canvasWinth;
    let y = Math.random() * canvasHeight;
    context.moveTo(x,y);
    context.lineTo(x+1, y+1);
    context.stroke();
  }
  return showNum.join('');
};
