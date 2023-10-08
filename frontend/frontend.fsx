open System
open System.Threading.Tasks
open System.Net
open System.Net.Sockets


let callRacketower (message: string) =
    task {
        let endpoint = IPEndPoint(IPAddress.Parse("127.0.0.1"), 8891)
        use client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.IP)
        do! client.ConnectAsync(endpoint)
        let serialized = System.Text.Encoding.UTF8.GetBytes(message)
        client.Send(serialized, serialized.Length, SocketFlags.None) |> ignore
        do! Task.Delay 1000
        let buffer = [| for _ in 0..20 do 0uy |]
        let receivedBytes = client.Receive buffer
        return System.Text.Encoding.UTF8.GetString buffer, receivedBytes
    }
    |> Async.AwaitTask
    |> Async.RunSynchronously

System.Console.ReadLine()
|> callRacketower
|> printfn "%A"    