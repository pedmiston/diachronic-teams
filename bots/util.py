def get_as_list(data, key, default=None):
    result = data.get(key, default)
    if not isinstance(result, list):
        result = [result]
    return result
