require 'bb-ruby'

class PostController < ApplicationController
  include PostHelper

  def new
    @vars = {}
    # TODO: Check base_title/body access rights
    # Check that 1) User belongs to clan owning the forum
    # or 2) User belongs to allied clan AND forum is public for alliance
    @vars[:base_title] = @vars[:base_body] = ''
    unless params[:reply_to].nil?
      base_post = PostModel.read_one(session[:user_clan],
                                     params[:thread_id],
                                     params[:reply_to])
      unless base_post.nil?
        @vars[:base_title] = base_post['title']
        @vars[:base_body] = quote(base_post['body'])
      end
    end
  end

  def create
    limit = 5
    if params[:body].length < limit
      return redirect_to new_post_path,
                         :alert => t('app.forums.body_empty', :limit => limit)
    end
    post_fields = {:body => params[:body],
                   :title => params[:title],
                   :clan_id => session[:user_clan],
                   :thread_id => params[:thread_id],
                   :created_by => session[:user_account] }
    PostModel.create(session[:user_clan],
                     params[:thread_id],
                     post_fields)
    redirect_to thread_path(:id => params[:thread_id])
  end
end
