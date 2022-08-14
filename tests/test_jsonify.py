from unidef.languages.javascript.jsonify import Jsonify, JsonObject, JsonProperty, JsonRawValue


def test_jsonify():
    jsonify = Jsonify()
    assert jsonify({}) == JsonObject([])
    assert jsonify({
        'test': 1
    }) == JsonObject([JsonProperty(JsonRawValue('test'), JsonRawValue(1))])
