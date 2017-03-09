((scope) => {
  const ws = new scope.WebSocket('ws://localhost:9000/updates')

  let count = 0

  const onmessage = (event) => {
    const data = JSON.parse(event.data)

    if (data.msg === "Individual"){

      const img = document.getElementById('img' + (count % 4))

      img.src = `data:image/jpeg;base64,${data.image}`

      count = count + 1
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
