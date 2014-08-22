# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).
#
# Examples:
#
#   cities = City.create([{ name: 'Chicago' }, { name: 'Copenhagen' }])
#   Mayor.create(name: 'Emanuel', city: cities.first)

# Force-decorate the User class in case it hasn't been yet. Fixes #495.
Forem.decorate_user_class!

Forem::Category.create(:id => 1, :name => 'clan-bad1')
Forem::Category.create(:id => 2, :name => 'clan-good')
Forem::Category.create(:id => 3, :name => 'clan-bad2')

user = Forem.user_class.first
unless user.nil?
  forum1 = Forem::Forum.find_or_create_by(
      :category_id => 1, #Forem::Category.find_by(:name => 'clan-good').id,
      :name => "Clan", :description => "Clan news, announcements, clan-related topics")
  forum2 = Forem::Forum.find_or_create_by(
      :category_id => 1, #Forem::Category.find_by(:name => 'clan-good').id,
      :name => "Off-topic", :description => "Everything else, daily chit-chat")
  forum_bad1 = Forem::Forum.find_or_create_by(
      :category_id => 2, #Forem::Category.find_by(:name => 'clan-bad1').id,
      :name => "Bad1 Forum", :description => "Shound not see this")
  forum_bad2 = Forem::Forum.find_or_create_by(
      :category_id => 3, #Forem::Category.find_by(:name => 'clan-bad2').id,
      :name => "Bad2 Forum", :description => "Shound not see this")

  post1 = Forem::Post.find_or_initialize_by(text: "Hello Clan Forum1")
  post1.user = user

  topic1 = Forem::Topic.find_or_initialize_by(subject: "Welcome to Clan Forum1")
  topic1.forum = forum1
  topic1.user = user
  topic1.posts = [ post1 ]
  topic1.save!

  postb1 = Forem::Post.find_or_initialize_by(text: "Hello Bad Forum1 :(")
  postb1.user = user

  topicb1 = Forem::Topic.find_or_initialize_by(subject: "Welcome to Bad Forum1 :(")
  topicb1.forum = forum_bad1
  topicb1.user = user
  topicb1.posts = [ postb1 ]
  topicb1.save!
end
