# Import the keyring module.
import gnomekeyring as gk

def get_gk_pass(keyring_name, key):
    itemkeys = gk.list_item_ids_sync(keyring_name)

    # Loop through keys, get key name and test against input. 
    for keyid in itemkeys:
        item_info = gk.item_get_info_sync(keyring_name, keyid)

        name = item_info.get_display_name()

        if name == key:
            return item_info.get_secret()

if __name__ == "__main__":
    print get_gk_pass('login', 'sontek@gmail.com'),
