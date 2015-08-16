/** @jsx React.DOM */

var App = React.createClass({
  getInitialState: function () {
    return {
      'source_code': '',
      'process_name': 'xxx',
    }
  },
  handleForward: function (e) {
    var self = this;
    e.preventDefault();
    $.ajax({
      url: "/process/" + self.state.process_name + "/f",
      type: "POST",
      contentTye: "application/json",
      dataType: "json"
    }).success(function (body) {
      console.log(body);
    });
  },
  handleStartDebug: function (e) {

    var self = this;
    e.preventDefault();
    var source_code = $('textarea.source_code').val();

    $.ajax({
      url : "/debug",
      type: "POST",
      data: JSON.stringify({
        source_code: source_code,
      }),
      contentType: "application/json",
      dataType   : "json",
    }).success(function (body) {
      console.log('body.process_name', body.process_name);
      self.setState({
        process_name: body.process_name,
        source_code: body.source_code,
      });
    });

  },
  render: function () {
  	var self = this;
    return (
      <form>
        <div>
          <textarea className="source_code" style={{width: '80%', height: '10em'}}></textarea>
        </div>
        <button onClick={self.handleStartDebug}>debug</button>
        <pre style={{border: '1px solid pink'}} className="source_code">{self.state.source_code}</pre>
        <button onClick={self.handleForward}>forward</button>
        <button onClick={self.handleBackward}>backward</button>
      </form>
    );
  }
});

React.render(<App />, document.body);