import React from "react";
import TaskStore from "../stores/TaskStore";

export default React.createClass({
  displayName: "NewTaskForm",
  getInitialState() {
    return {
      description: ""
    };
  },
  onDescriptionChange(event) {
    this.setState({
      description: event.target.value
    });
  },
  onSubmit(event) {
    event.preventDefault();
    TaskStore.Actions.createTask(this.state.description);
    // Clear field
    this.setState({
      description: ""
    });
  },
  render() {
    // Should the Add button be disabled?
    let disabled = (this.state.description === "");
    // Render
    return (
      <form className="form-inline" role="form" onSubmit={this.onSubmit}>
        <div className="form-group">
          <input type="text"
                 className="form-control"
                 placeholder="Enter task description"
                 value={this.state.description}
                 onChange={this.onDescriptionChange} />
        </div>
        {"\u2000"}
        <button type="submit" disabled={disabled} className="btn btn-primary">Add</button>
      </form>
    );
  }
});
