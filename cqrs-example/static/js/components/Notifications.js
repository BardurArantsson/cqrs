import React from "react";
import RefluxMixin from "../mixins/RefluxMixin";
import NotificationStore from "../stores/NotificationStore";
import classNames from "classnames";

export default React.createClass({
  displayName: "Notifications",
  mixins: [ RefluxMixin ],
  componentWillMount() {
    this.listenTo(NotificationStore.Store);
  },
  render() {
    // Render the notifications
    let notifications = NotificationStore.Store.getNotifications().map(notification => {
      let style = {
        "opacity": 1 - notification.expiryFraction
      };
      let classes = classNames({
        "notifications-item": true,
        "alert-success": !notification.error,
        "alert-danger": !!notification.error
      });
      return (
        <div className={classes} key={notification.id} role={alert} style={style}>
          {notification.message}
        </div>
      );
    });

    return (
      <div className="notifications-container">
        {notifications}
      </div>
    );
  }
});
