"""Create innovation recipes from the combinations answer key."""
from sys import stdout
import pandas

initial_resources = {1: 'Big_Tree', 2: 'Tree', 3: 'Stone',
                     4: 'Red_Berry', 5: 'Blue_Berry', 6: 'Antler'}

def make_recipes():
    answer_key = pandas.read_csv('answer_key.csv')
    item_labels = make_item_labels(answer_key)
    recipes = answer_key.apply(to_recipe, axis=1, item_labels=item_labels)
    return recipes

def make_item_labels(answer_key):
    # Start with initial resources and update with innovations
    item_labels = initial_resources.copy()
    # Item labels are item names without image file extentions
    answer_key['Label'] = answer_key.Name.str.split('.').str[0]
    item_labels.update(answer_key.set_index('Number')['Label'].to_dict())
    return item_labels

def to_recipe(innovation, item_labels):
    required_items = [item_number for item_number in
                      innovation[['Item1', 'Item2', 'Item3', 'Item4']].tolist()
                      if item_number != 0]
    required_labels = ' + '.join([item_labels[n] for n in required_items])
    result_label = item_labels[innovation.Number]
    return ' = '.join([required_labels, result_label])

if __name__ == '__main__':
    recipes = make()
    for recipe in recipes:
        stdout.write(recipe + '\n')
