import Reflux from "reflux";
import EventSourceStore from "./EventSourceStore";

let createTask = Reflux.createAction();
let updateTaskStatus = Reflux.createAction();
let archiveCompletedTasks = Reflux.createAction();

let Store = Reflux.createStore({
  /** @protected */
  init() {
    // Initial state.
    this.tasks = [ ];
    // Register actions.
    this.listenTo(createTask, this.onCreateTask);
    this.listenTo(updateTaskStatus, this.onUpdateTaskStatus);
    this.listenTo(archiveCompletedTasks, this.onArchiveCompletedTasks);
    // Subscribe to all notifications: Regardless of what's going
    // on we just re-request the list of tasks. (Since we know all
    // notifications are going to be the result of some
    // modification of the task list).
    this.listenTo(EventSourceStore.Store, this.onNotifications);
    // Refresh list of tasks
    this.doRefresh();
  },
  /**
   * @private
   */
  doRefresh() {
    // No longer have a pending refresh.
    this.pending = false;
    // Fetch tasks.
    this.ajaxGetTasks();
  },
  /**
   * @private
   */
  reportError(error) {
    // These notifications should really not expire (i.e. require
    // an explicit click), but, again, this is just an example.
    NotificationStore.Actions.addNotification({
      type: "client-error",
      message: "Oops. There was an error communicating with the server. Message was '" + error.message + "'"
    });
  },
  /**
   * @private
   */
  onNotifications(notifications) {
    // If a refresh is already pending, then we don't need to do anything
    // and can just throw the new notifications away.
    if (this.pending) {
      return;
    }
    // Check for a refresh.
    let refresh = notifications.some(notification => notification.type === "refresh");
    if (!refresh) {
      return;
    }
    // We don't have a refresh pending, so let's set it up. We use
    // the timeout to hopefully avoid either a) spamming the
    // server with requests when receiving updates from "large"
    // commands which generate lots of notifications, or b)
    // initiating/canceling lots of Ajax requests in the browser.
    setTimeout(this.doRefresh, 50);
  },
  /**
   * @private
   */
  ajaxGetTasks() {
    // Request a fresh list of tasks.
    superagent
      .get("/tasks")
      .end(response => {
        // Replace current list of tasks
        this.tasks = response.body.map(task => {
          return {
            status: task.done,
            id: task.id,
            description: task.title
          };
        });
        // Emit a change event
        this.trigger();
      });
  },
  /**
   * @private
   */
  onCreateTask(description) {
    // Post the request. We'll automatically get a notification of
    // changes through the EventSource, so we don't expect any
    // direct response.
    superagent
      .post("/tasks")
      .query(
        { title: description })
      .end(response => {
        if (response.error) {
          this.reportError(response.error);
        }
      });
  },
  /**
   * @private
   */
  onUpdateTaskStatus(id, status) {
    // Use the correct update URL depending on new status.
    let url = status ? "/tasks/complete" : "/tasks/reopen";
    // Update the appropriate task.
    superagent
      .post(url)
      .query(
        { id: id })
      .end(response => {
        if (response.error) {
          this.reportError(response.error);
        }
      });
  },
  /**
   * @private
   */
  onArchiveCompletedTasks() {
    // Update tasks
    superagent
      .post("/tasks/archive-completed")
      .end(response => {
        if (response.error) {
          this.reportError(response.error);
        }
      });
  },
  /**
   * Get the number of completed tasks.
   *
   * @public
   */
  getCompletedCount() {
    var count = 0;
    for (let task of this.tasks) {
      if (task.status === true) {
        count++;
      }
    };
    return count;
  },
  /**
   * Get the array of tasks.
   *
   * @public
   */
  getTasks() {
    return this.tasks;
  }
});

// Exports
export default {
  Actions: {
    createTask: createTask,
    updateTaskStatus: updateTaskStatus,
    archiveCompletedTasks: archiveCompletedTasks
  },
  Store: Store
};
