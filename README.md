[![progress-banner](https://backend.codecrafters.io/progress/bittorrent/bbff74fa-d87d-4c03-ab34-0de90200fd73)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

This is a starting point for Haskell solutions to the
["Build Your Own BitTorrent" Challenge](https://app.codecrafters.io/courses/bittorrent/overview).

In this challenge, you’ll build a BitTorrent client that's capable of parsing a
.torrent file and downloading a file from a peer. Along the way, we’ll learn
about how torrent files are structured, HTTP trackers, BitTorrent’s Peer
Protocol, pipelining and more.

**Note**: If you're viewing this repo on GitHub, head over to
[codecrafters.io](https://codecrafters.io) to try the challenge.

# Passing the first stage

The entry point for your BitTorrent implementation is in `app/Main.hs`. Study
and uncomment the relevant code, and push your changes to pass the first stage:

```sh
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```

Time to move on to the next stage!

# Stage 2 & beyond

Note: This section is for stages 2 and beyond.

1. Ensure you have `stack` installed locally
1. Run `./your_bittorrent.sh` to run your program, which is implemented in
   `app/Main.hs`.
1. Commit your changes and run `git push origin master` to submit your solution
   to CodeCrafters. Test output will be streamed to your terminal.

## Notes

- started this project with the intent of getting familiar with megaparser and argparse,
  I had used argparse in the grep project but not argparse. I made a half decent start.
  Got familiar with the parser but when I ran into the hash values i realised my mistake
  I had built the parsers and adt's with String out of ignorance and lazyness.

- This was a painful realisation I now have to rewrite most of the functions to use
  bytestring instead, its a significant rewrite especially when considering,
  I was pushing my haskell skills to the limit to get this far. This rewrite
  will be a significant undertaking for me.
