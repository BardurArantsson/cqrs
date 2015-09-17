import React from "react";
import TaskStore from "../stores/TaskStore";
import RefluxMixin from "../mixins/RefluxMixin";
import classNames from "classnames";

export default React.createClass({
  displayName: "TaskSummary",
  mixins: [ RefluxMixin ],
  componentWillMount() {
    this.listenTo(TaskStore.Store);
  },
  hasCompletedTasks() {
    return TaskStore.Store.getCompletedCount() > 0;
  },
  onArchiveCompleted() {
    if (this.hasCompletedTasks()) {
      TaskStore.Actions.archiveCompletedTasks();
    }
  },
  renderCompletedCountMessage() {
    let count = TaskStore.Store.getCompletedCount();
    if (count === 1) {
      return "1 task marked completed";
    } else {
      return "" + count + " tasks marked completed";
    }
  },
  render() {
    let className = classNames({
      "text-muted": !this.hasCompletedTasks()
    });
    let message = this.renderCompletedCountMessage();
    return (
      <span>(
        {message} <a
          href="#"
          className={className}
          onClick={this.onArchiveCompleted}>
          Archive
        </a>
      )</span>
    );
  }
});
