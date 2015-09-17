import Reflux from "reflux";

let start = Reflux.createAction();

// Create the store.
let Store = Reflux.createStore({
  /** @protected */
  init() {
    this.listenTo(start, this.onStart);
    this.eventSource = null;
  },
  /** @private */
  onStart() {
    this.eventSource = new EventSource("/events");
    this.eventSource.onmessage = (msg => {
      let data = JSON.parse(msg.data);
      this.trigger(data.notifications);
    });
  }
});

// Start the event source asynchronously. We delay to allow full
// initialization.
start();

// Export
export default {
  Store: Store,
  Actions: {
    // Does not export any actions; it's just a source of events.
  }
};
