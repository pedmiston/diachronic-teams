def get_as_list(data, key, default=None):
    data = self._data.get(key, default)
    if not isinstance(data, list):
        data = [data]
    return data
