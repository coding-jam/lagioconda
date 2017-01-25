((scope) => {
  const ws = new scope.WebSocket('wss://echo.websocket.org')
  console.log(ws)
})(window)
