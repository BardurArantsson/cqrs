/**
 * Get/create the list of subscriptions for the given component
 * instance.
 *
 * @private
 */
function getSubscriptions(self) {
  self.__subscriptions = self.__subscriptions || [ ];
  return self.__subscriptions;
}

/**
 * Reflux mixin which gives a simple way to subscribe to notifications
 * for any store change.
 *
 * Values emitted by the store are not stored.
 */
export default {

  /**
   * Listen to all updates triggered in a given store and force a
   * component re-render whenever the store's state changes.
   *
   * @public
   */
  listenTo(store) {
    getSubscriptions(this).push(store.listen(() => {
      // Since we're asynchronous, we need to check if the component
      // is actually still mounted at the time we get the callback.
      if (this.isMounted()) {
        this.forceUpdate();
      }
    }));
  },

  /**
   * @protected
   */
  componentWillUnmount() {
    for (let unsubscribe of getSubscriptions(this)) {
      unsubscribe();
    };
  }

};
