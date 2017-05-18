((scope) => {
  const ws = new scope.WebSocket('ws://localhost:9000/updates')

  let count = 0

  const onmessage = (event) => {
    const data = JSON.parse(event.data)

    if (data.msg === "IndividualInfo"){

      const img = document.getElementById('img' + data.population)

      img.src = `data:image/jpeg;base64,${data.image}`
      const lab = document.getElementById('pop'+ data.population)
      lab.innerHTML = data.info

    }
    if (data.msg === "Statistics") {
        const statistics = document.getElementById('statistics')
        statistics.innerHTML = data.message
    }

  }

  const onopen = (event) => {
    document.querySelector('span[data-status]').innerHTML = 'Connected'
  }

  ws.onmessage = onmessage
  ws.onopen = onopen

var message = {
  "msg": "start"
};


  scope.send = () => {
    //ws.send('Start')
    ws.send(JSON.stringify(message));
  }
})(window)
