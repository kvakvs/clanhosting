class ForumController < ApplicationController
  def index
    @vars = {}
    @vars[:clan_id]   = Integer(params[:clan_id])
    @vars[:clan_info] = ClanModel.clan_info(@vars[:clan_id])
    @vars[:forums]    = ForumModel::list(session[:user_clan])
  end

  def show
    clan_id = session[:user_clan]
    forum_id = params[:id]

    @vars = {}
    @vars[:forum] = ForumModel.read(clan_id, forum_id)
    return redirect_to(forum_index_path,
                       :alert => t('app.forums.forum_not_found')
                      ) unless @vars[:forum]

    @vars[:threads] = ThreadModel.list(clan_id, forum_id)
  end
end
