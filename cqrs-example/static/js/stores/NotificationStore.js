import Reflux from "reflux";
import EventSourceStore from "./EventSourceStore";

/**
 * Generate a fresh identifier every time this function is called.
 */
let getFreshId = (function () {
  var currentId = 0;
  return function() {
    currentId++;
    return currentId.toString();
  };
}());

/**
 * Delay before notifications expire
 */
const expiryDelay = 2000;

/**
 * Delay before notifications fade from view
 */
const fadeDelay = 1000;

/**
 * Actions supported by the store.
 */
let addNotificationAction = Reflux.createAction();

/**
 * Store
 */
let Store = Reflux.createStore({
  /** @protected */
  init() {
    // No notifications to start with.
    this.notifications = [ ];
    // Expire notifications every 100ms. We should probably avoid
    // doing anything at all if there are no outstanding
    // notifications, but this'll do for a demo.
    window.setInterval(this.expireNotifications.bind(this), 100);
    // Listen for all notifications.
    this.listenTo(addNotificationAction, this.onNotification);
    this.listenTo(EventSourceStore.Store, this.onNotifications);
  },
  /** @private */
  expireNotifications() {
    // Any changes?
    var changed = false;
    // Current time in milliseconds
    let now = Date.now();
    // Go through all the notifications and expire.
    let filteredNotifications = this.notifications.filter(notification =>
      notification.received > now - expiryDelay);
    changed = changed || (filteredNotifications.length !== this.notifications.length);
    // Go through all the remaining notifications to set their relative expiry time.
    for (let notification of filteredNotifications) {
      let f = Math.max(0.0, Math.min(1.0, (now - notification.received - fadeDelay) / (expiryDelay - fadeDelay)));
      if (f !== notification.expiryFraction) {
        notification.expiryFraction = f;
        changed = true;
      }
    };
    // Update the stored notifications
    this.notifications = filteredNotifications;
    // Emit a change event if there were any changes.
    if (changed) {
      this.trigger();
    }
  },
  /**
   * @private
   */
  addNotification(notification) {
    // Add to the current list. We add at the start so we'll have
    // a list sorted in reverse chronological order.
    this.notifications.unshift({
      id: getFreshId(),
      notification: notification,
      expiryFraction: 1.0,
      received: Date.now()
    });
    // Emit a change event
    this.trigger();
  },
  /**
   * @private
   */
  onNotification(notification) {
    var coalesced;
    var changed = false;
    // We want different behavior according to notification type.
    switch (notification.type)
    {
      case "added-task": {
        // Just append a notification. We don't bother coalescing
        // these since they're usually one-at-a-time anyway.
        this.addNotification(notification);
        changed = true;
        break;
      }

      case "archived-task": {
        // See if we can coalesce the given notification with any
        // existing one.
        coalesced = false;
        for (let existingNotification of this.notifications) {
          // If there's an existing notification which is
          // appropriate, then we just add into that.
          if (!coalesced && existingNotification.notification.type === "archived-task") {
            // If it's not yet started expiring, then we just add to its counter.
            if (existingNotification.expiryFraction >= 1.0) {
              existingNotification.notification.count += 1;
              coalesced = true;
            }
          }
        };
        // If we didn't combine into some other notification, we
        // need to augment the notification with a count and add
        // it.
        if (!coalesced) {
          notification = Object.assign({ }, notification, { count: 1 });
          this.addNotification(notification);
        }
        changed = true;
        break;
      }

      case "client-error": {
        // Just append the error notification. It doesn't make much
        // sense to try to coalesce.
        this.addNotification(notification);
        changed = true;
        break;
      }
    }

    // If we had changes, we emit an event
    if (changed) {
      this.trigger();
    }
  },
  /**
   * @private
   */
  onNotifications(notifications) {
    notifications.forEach(this.onNotification);
  },
  /**
   * Get the current list of notifications.
   *
   * @public
   */
  getNotifications() {
    var message;

    var renderedMessages = [ ];
    for (let notification of this.notifications) {
      switch (notification.notification.type)
      {
        case "added-task": {
          renderedMessages.push({
            error: false,
            id: notification.id,
            message: "Added task '" + notification.notification.title + "'",
            expiryFraction: notification.expiryFraction
          });
          break;
        }

        case "archived-task": {

          message = (notification.notification.count === 1) ?
            "Archived 1 task" :
            "Archived " + notification.notification.count + " tasks";

          renderedMessages.push({
            error: false,
            id: notification.id,
            message: message,
            expiryFraction: notification.expiryFraction
          });
          break;
        }

        case "client-error": {
          renderedMessages.push({
            error: true,
            id: notification.id,
            message: notification.notification.message,
            expiryFraction: notification.expiryFraction
          });
          break;
        }
      }
    };

    return renderedMessages;
  }

});

export default {
  Store: Store,
  Actions: {
    addNotification: addNotificationAction
  }
};
