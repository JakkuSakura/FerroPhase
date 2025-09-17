using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
public class Point
{
    [JsonPropertyName("x")]
    public double x { get; set; }
    [JsonPropertyName("y")]
    public double y { get; set; }

    public Point()
    {
    }

    public Point(double x, double y)
    {
        this.x = x;
        this.y = y;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static Point? FromJson(string json)
    {
        return JsonSerializer.Deserialize<Point>(json);
    }
}
public class Color
{
    [JsonPropertyName("r")]
    public byte r { get; set; }
    [JsonPropertyName("g")]
    public byte g { get; set; }
    [JsonPropertyName("b")]
    public byte b { get; set; }

    public Color()
    {
    }

    public Color(byte r, byte g, byte b)
    {
        this.r = r;
        this.g = g;
        this.b = b;
    }

    // JSON Serialization Methods
    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static Color? FromJson(string json)
    {
        return JsonSerializer.Deserialize<Color>(json);
    }
}
public class Program
{
    public static void Main(string[] args)
    {
        const int POINT_SIZE = 16;
        const int TOTAL_SIZE = 40;
        const int COLOR_SIZE = 24;

        // Example Point instantiation
        var point_instance = new Point()
        {
            x = 0.0,
            y = 0.0,
        };

        // Example Color instantiation
        var color_instance = new Color()
        {
            r = 0,
            g = 0,
            b = 0,
        };

        // Generated output
        Console.WriteLine("Transpilation Example");
        Console.WriteLine($"POINT_SIZE: {POINT_SIZE}");
        Console.WriteLine($"TOTAL_SIZE: {TOTAL_SIZE}");
        Console.WriteLine($"COLOR_SIZE: {COLOR_SIZE}");
    }
}
