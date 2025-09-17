using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
public enum EventType
{
    Unknown,
    Data,
    Notification,
    Command,
    Response,
    Error
}
public enum NotificationLevel
{
    Debug,
    Info,
    Warning,
    Error,
    Critical
}
public class DataPoint
{
    [JsonPropertyName("value")]
    public double value { get; set; }
    [JsonPropertyName("amount")]
    public double amount { get; set; }
    [JsonPropertyName("category")]
    public string category { get; set; }

    public DataPoint()
    {
    }

    public DataPoint(double value, double amount, string category)
    {
        this.value = value;
        this.amount = amount;
        this.category = category;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static DataPoint? FromJson(string json)
    {
        return JsonSerializer.Deserialize<DataPoint>(json);
    }
}
public class Event
{
    [JsonPropertyName("id")]
    public string id { get; set; }
    [JsonPropertyName("value")]
    public double value { get; set; }
    [JsonPropertyName("amount")]
    public double amount { get; set; }
    [JsonPropertyName("metadata")]
    public string metadata { get; set; }
    [JsonPropertyName("status")]
    public string status { get; set; }
    [JsonPropertyName("created_at")]
    public ulong created_at { get; set; }
    [JsonPropertyName("updated_at")]
    public ulong updated_at { get; set; }
    [JsonPropertyName("received_at")]
    public ulong received_at { get; set; }

    public Event()
    {
    }

    public Event(string id, double value, double amount, string metadata, string status, ulong created_at, ulong updated_at, ulong received_at)
    {
        this.id = id;
        this.value = value;
        this.amount = amount;
        this.metadata = metadata;
        this.status = status;
        this.created_at = created_at;
        this.updated_at = updated_at;
        this.received_at = received_at;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static Event? FromJson(string json)
    {
        return JsonSerializer.Deserialize<Event>(json);
    }
}
public class NotificationEvent
{
    [JsonPropertyName("id")]
    public string id { get; set; }
    [JsonPropertyName("timestamp")]
    public ulong timestamp { get; set; }
    [JsonPropertyName("message")]
    public string message { get; set; }
    [JsonPropertyName("level")]
    public NotificationLevel level { get; set; }

    public NotificationEvent()
    {
    }

    public NotificationEvent(string id, ulong timestamp, string message, NotificationLevel level)
    {
        this.id = id;
        this.timestamp = timestamp;
        this.message = message;
        this.level = level;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static NotificationEvent? FromJson(string json)
    {
        return JsonSerializer.Deserialize<NotificationEvent>(json);
    }
}
public class CommandEvent
{
    [JsonPropertyName("id")]
    public string id { get; set; }
    [JsonPropertyName("timestamp")]
    public ulong timestamp { get; set; }
    [JsonPropertyName("command")]
    public string command { get; set; }
    [JsonPropertyName("parameters")]
    public List<string> parameters { get; set; }

    public CommandEvent()
    {
    }

    public CommandEvent(string id, ulong timestamp, string command, List<string> parameters)
    {
        this.id = id;
        this.timestamp = timestamp;
        this.command = command;
        this.parameters = parameters;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static CommandEvent? FromJson(string json)
    {
        return JsonSerializer.Deserialize<CommandEvent>(json);
    }
}
public class ResponseEvent
{
    [JsonPropertyName("id")]
    public string id { get; set; }
    [JsonPropertyName("timestamp")]
    public ulong timestamp { get; set; }
    [JsonPropertyName("status_code")]
    public ushort status_code { get; set; }
    [JsonPropertyName("data")]
    public List<byte> data { get; set; }

    public ResponseEvent()
    {
    }

    public ResponseEvent(string id, ulong timestamp, ushort status_code, List<byte> data)
    {
        this.id = id;
        this.timestamp = timestamp;
        this.status_code = status_code;
        this.data = data;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static ResponseEvent? FromJson(string json)
    {
        return JsonSerializer.Deserialize<ResponseEvent>(json);
    }
}
public class Configuration
{
    [JsonPropertyName("min_value")]
    public double min_value { get; set; }
    [JsonPropertyName("max_value")]
    public double max_value { get; set; }
    [JsonPropertyName("default_amount")]
    public double default_amount { get; set; }
    [JsonPropertyName("created_at")]
    public ulong created_at { get; set; }
    [JsonPropertyName("updated_at")]
    public ulong updated_at { get; set; }
    [JsonPropertyName("received_at")]
    public ulong received_at { get; set; }

    public Configuration()
    {
    }

    public Configuration(double min_value, double max_value, double default_amount, ulong created_at, ulong updated_at, ulong received_at)
    {
        this.min_value = min_value;
        this.max_value = max_value;
        this.default_amount = default_amount;
        this.created_at = created_at;
        this.updated_at = updated_at;
        this.received_at = received_at;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static Configuration? FromJson(string json)
    {
        return JsonSerializer.Deserialize<Configuration>(json);
    }
}
public class EventCollection
{
    [JsonPropertyName("created_at")]
    public ulong created_at { get; set; }
    [JsonPropertyName("updated_at")]
    public ulong updated_at { get; set; }
    [JsonPropertyName("received_at")]
    public ulong received_at { get; set; }
    [JsonPropertyName("source")]
    public string source { get; set; }
    [JsonPropertyName("events")]
    public List<Event> events { get; set; }

    public EventCollection()
    {
    }

    public EventCollection(ulong created_at, ulong updated_at, ulong received_at, string source, List<Event> events)
    {
        this.created_at = created_at;
        this.updated_at = updated_at;
        this.received_at = received_at;
        this.source = source;
        this.events = events;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static EventCollection? FromJson(string json)
    {
        return JsonSerializer.Deserialize<EventCollection>(json);
    }
}
public class EventBatch
{
    [JsonPropertyName("processed")]
    public byte processed { get; set; }
    [JsonPropertyName("events")]
    public List<SystemEvent> events { get; set; }

    public EventBatch()
    {
    }

    public EventBatch(byte processed, List<SystemEvent> events)
    {
        this.processed = processed;
        this.events = events;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static EventBatch? FromJson(string json)
    {
        return JsonSerializer.Deserialize<EventBatch>(json);
    }
}
public class Program
{
    public static void Main(string[] args)
    {
        const int COLOR_SIZE = 24;
        const int TOTAL_SIZE = 40;
        const int POINT_SIZE = 16;

        // Example DataPoint instantiation
        var datapoint_instance = new DataPoint()
        {
            value = 0.0,
            amount = 0.0,
            category = "",
        };

        // Example Event instantiation
        var event_instance = new Event()
        {
            id = "",
            value = 0.0,
            amount = 0.0,
            metadata = "",
            status = "",
            created_at = 0,
            updated_at = 0,
            received_at = 0,
        };

        // Example NotificationEvent instantiation
        var notificationevent_instance = new NotificationEvent()
        {
            id = "",
            timestamp = 0,
            message = "",
            level = null,
        };

        // Example CommandEvent instantiation
        var commandevent_instance = new CommandEvent()
        {
            id = "",
            timestamp = 0,
            command = "",
            parameters = new List<string>{},
        };

        // Example ResponseEvent instantiation
        var responseevent_instance = new ResponseEvent()
        {
            id = "",
            timestamp = 0,
            status_code = 0,
            data = new List<byte>{},
        };

        // Example Configuration instantiation
        var configuration_instance = new Configuration()
        {
            min_value = 0.0,
            max_value = 0.0,
            default_amount = 0.0,
            created_at = 0,
            updated_at = 0,
            received_at = 0,
        };

        // Example EventCollection instantiation
        var eventcollection_instance = new EventCollection()
        {
            created_at = 0,
            updated_at = 0,
            received_at = 0,
            source = "",
            events = new List<Event>{},
        };

        // Example EventBatch instantiation
        var eventbatch_instance = new EventBatch()
        {
            processed = 0,
            events = new List<SystemEvent>{},
        };

        // Generated output
        Console.WriteLine("Transpilation Example");
        Console.WriteLine($"COLOR_SIZE: {COLOR_SIZE}");
        Console.WriteLine($"TOTAL_SIZE: {TOTAL_SIZE}");
        Console.WriteLine($"POINT_SIZE: {POINT_SIZE}");
    }
}
