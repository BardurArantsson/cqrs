import React from "react";
import TaskStore from "../stores/TaskStore";

/**
 * Display a single task with a checkbox for "crossing off" the task.
 */
export default React.createClass({
  displayName: "Task",
  onChange(event) {
    let task = this.props.task;
    TaskStore.Actions.updateTaskStatus(task.id, event.target.checked);
  },
  renderTaskDescription() {
    let task = this.props.task;
    if (task.status) {
      return (<s>{task.description}</s>);
    } else {
      return task.description;
    }
  },
  render() {
    let task = this.props.task;
    // Render description
    let description = this.renderTaskDescription();
    // Prevent accidental selection of the task description.
    let style = {
      "userSelect": "none"
    };
    // Render the task
    return (
      <li>
        <div className="checkbox" style={style}>
          <label>
            <input type="checkbox" checked={task.status} onChange={this.onChange} />
            {description}
          </label>
        </div>
      </li>
    );
  }
});
