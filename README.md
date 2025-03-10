# Charapainter

[3.99$ currently on the AppStore](https://apps.apple.com/nl/app/charapainter/id6739626259?l=en-GB&mt=12)

You can build & use it your own if you can. There are also debug buid in Release, but they may not signed.

The code relying on [LispWorks](https://www.lispworks.com) (proprietary) to work.

Dependencies (Common Lisp package):

- alexandria
- anaphora
- serapeum

Load the `charapainter` system, evaluate `(capi:contain (make-instance 'charapainter::main-interface))` to show the main interface.

And we use [icons8](https://icons8.com) for tool icons.

## Promotion text

Drawing & converting images with colorful characters. Export to image, HTML and terminal

## Description

Charapainter is an application for creating colorful art using characters.

We support:

- Full editing features: shapes, lines, arrows, selection;
- Multiple layers, transparency and alpha blending, for both foreground and background
- Terminal-compliant 4-bit, 8-bit and 24-bit colors, for both foreground and background
- Bold, Italic and Underlined font styles
- Unicode dual-width characters (partial Emoji support)
- Export/Copy the artwork to HTML, ANSI escaped sequences, plain text, and images (PNG, JPEG, BMP, TIFF)
- Convert existing images into character art, in many creative styles

The application is focus and tidy, no useless features, no cloud, no AI, no internet connection needed. One-time purchase, lifetime support.

Contact the developer anytime if you want more features! :D

# Previews

![preview 1](./res/0.2preview1.png)
![preview 2](./res/0.2preview2.png)
![preview 3](./res/0.2preview3.png)

# License

Any files created by me (us) in this repo is 0BSD licensed.

> Note that the pictures prefixing with `icons8-` under the `./res/` folder are icon resources provided by [icons8](https://icons8.com), which is not part of my work.

---

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE
FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
