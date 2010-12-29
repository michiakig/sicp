require 'rubygems'
require 'nokogiri'

infile = ARGV[0]
outfile = ARGV[1]
#listfile = ARGV[2]

README = "# Structure and Interpretation of Computer Programs\n\nI'm reading [SICP](http://mitpress.mit.edu/sicp/), watching the [(1986) video lectures](http://groups.csail.mit.edu/mac/classes/6.001/abelson-sussman-lectures/), and trying to complete as many of the programming exercises as I can.\n\nThe following list was extracted from the online version of the book. It was a quick and dirty job but gives an overview of which exercises I've completed.\n\n"

# chapter titles
chapters = ["1 Building Abstractions with Procedures",
            "2 Building Abstractions with Data",
            "3 Modularity, Objects, and State",
            "4 Metalinguistic Abstraction",
            "5 Computing with Register Machines"]

completed = Hash.new

done = `grep -r "Exercise " ch*`.to_a.map do |x|
  mtch = x.match(/Exercise ((\d)\.(\d( |$)|\d\d))/)
  if(mtch)
    if(mtch[3].match(/0\d/))
      mtch[2] + '.' + mtch[3].gsub(/0/,'')
    else
      mtch[2] + '.' + mtch[3]
    end
  end
end
done.each { |x| completed[x] = true; }

#File.open(listfile) do |f|
#  while(line = f.gets)
#    if(split.length > 1)
#      completed[split[1]] = true
#    end
#  end
#end

doc = ''
File.open(infile) do |f|
  doc = Nokogiri::HTML(f)
end

File.open(outfile, 'w') do |f|
  f.write(README)
  chapter = '0'
  # all of exercises are in paragraphs
  ps = doc.xpath('//p')
  ps.each do |p|
    f.write("\n")

    match = p.content.match(/Exercise ((\d\d|\d)\.(\d\d|\d))/);
    if(match != nil)
      ch = match[2]
      if(ch != chapter)
        chapter = ch
        # if so, output a chapter heading
        f.write("\#\# #{chapters[ch.to_i - 1]}\n")
      end
    end

    if(completed[match[1]])
      f.write('X ')
#      f.write('<strike>')
    end

    p.children.each do |c| 
      # check if we are in a new chapter
      # output elements as Markdown
      # keep some HTML: images, subscript, superscript
      case c.name
      when 'em'
        f.write('*')
        f.write(c.inner_html)
        f.write('*')
      when 'strong', 'b'
        f.write('**')
        f.write(c.inner_html)
        f.write('**')
      when 'br'
        f.write("\n\n")
      when 'a'
        f.write(c.inner_html) unless c.inner_html == ''
      when 'img'
        f.write("<img src='http://mitpress.mit.edu/sicp/full-text/book/#{c.attributes['src'].value}' />")
      when 'text'
        f.write(c.to_s.gsub(/(&nbsp;)+/,' ')) unless c.to_s.match(/^(\n+)$/)
      else
        f.write(c) 
      end
    end

    if(completed[match[1]])
#      f.write('</strike>')
    end

    f.write("\n")
  end
  f.write("\n")
end
