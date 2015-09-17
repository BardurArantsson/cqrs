import React from "react";
import TaskStore from "../stores/TaskStore";
import RefluxMixin from "../mixins/RefluxMixin";
import Task from "./Task";

export default React.createClass({
  displayName: "TaskList",
  mixins: [ RefluxMixin ],
  componentWillMount() {
    this.listenTo(TaskStore.Store);
  },
  render() {
    let tasks = TaskStore.Store.getTasks().map(task =>
      (<Task key={task.id} task={task} />));
    return (
      <ul className="list-unstyled">
        {tasks}
      </ul>
    );
  }
});
