class PostController < ApplicationController
  include PostHelper

  def new
    # TODO: Check base_title/body access rights
    @base_title = @base_body = ''
    unless params[:reply_to].nil?
      base_post = ForumPost.read_one(session[:user_clan],
                                     params[:thread_id],
                                     params[:reply_to])
      unless base_post.nil?
        @base_title = base_post['title']
        @base_body = quote(base_post['body'])
      end
    end
  end

  def create
    post_fields = {:body => params[:body],
                   :title => params[:title],
                   :clan_id => session[:user_clan],
                   :thread_id => params[:thread_id],
                   :created_by => session[:user_account] }
    ForumPost.create(session[:user_clan],
                     params[:thread_id],
                     post_fields)
  end
end