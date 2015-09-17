import React from "react";
import TaskSummary from "./TaskSummary";
import TaskList from "./TaskList";
import NewTaskForm from "./NewTaskForm";
import Notifications from "./Notifications";

export default React.createClass({
  render() {
    return (
      <div>
        <div className="container">
          <h1>Tasks</h1>
          <div className="row">
            <div className="col-md-12">
              <TaskSummary />
            </div>
          </div>
          <div className="row">
            <div className="col-md-12">
              <TaskList />
            </div>
          </div>
          <div className="row">
            <div className="col-md-12">
              <NewTaskForm />
            </div>
          </div>
        </div>
        <Notifications />
      </div>
    );
  }
});
